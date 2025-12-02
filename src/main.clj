(ns main
  (:require
   [babashka.cli :as cli]
   [babashka.fs :as fs]
   [babashka.http-client :as http]
   [cheshire.core :as json]
   [clojure.string :as str]
   [clojure.pprint :as pp]
   [progrock.core :as pr]
   [clojure-watch.core :refer [start-watch]]
   [clj-fuzzy.metrics :as fuzzy]
   [clojure.core.async
    :as async
    :refer [<! <!! >! >!! chan go go-loop close!]]
   [clojure.java.io :as io]
   [clojure.java.shell :as sh])
  (:gen-class))

(defn show-help
  [spec]
  (cli/format-opts (merge spec {:order (vec (keys (:spec spec)))})))

(defn api-key-path [] (str (fs/home) "/.bmm_token"))
(defn bonelab-path [] (str (fs/home) "/.bmm_bone_path"))
(defn bmm-db-path [] (str (fs/home) "/.bmm_db.json"))
(def bonelab-id 3809)

(def cli-spec
  {:spec
   {:help {:alias :h
           :desc "Show help"}
    :token {:desc "Mod.io OAuth 2 Token"}
    :path {:alias :p
           :desc "Bonelab mod folder path"}
    :threads {:alias :t
              :desc "Number of threads to use while downloading"}
    :update {:alias :u
             :desc "Download subscribed mods from Mod.io, and update them if they are out of date"}
    :subscribe {:alias :s
                :desc "Subscribe to mods that are on your disk, may not work for all mods\nIntended for subscribing to mods gotten from fusion"}}})

(defn parse-args "returns [opts token]" [args]
  (let [opts (cli/parse-opts args cli-spec)]
    (when (:help opts)
      (println "bmm -- a terminal application for managing bonelab mods")
      (println (show-help cli-spec))
      (throw (ex-info "" {})))

    (when (:token opts)
      (println "saving user data...")
      (fs/create-file ( api-key-path))
      (-> (fs/file ( api-key-path))
          (fs/write-lines [(:token opts)])))

    (when (:path opts)
      (println "saving user data...")
      (fs/create-file ( bonelab-path))
      (-> (fs/file ( bonelab-path))
          (fs/write-lines [(:path opts)])))

    (when-not (fs/exists? ( api-key-path))
      (throw (ex-info
              "Missing OAuth 2 access token.
Rerun the program with --token.
This will save it for future runs" {})))

    (when-not (fs/exists? ( bonelab-path))
      (throw (ex-info
              "Missing bonelab mod folder path.
Rerun the program with --path
This will save it for future runs" {})))

    [opts
     (-> (fs/file ( api-key-path )) (fs/read-all-lines) (first))
     (-> (fs/file ( bonelab-path )) (fs/read-all-lines) (first))]))

(defn http-retry [method uri opts]
  (let [resp (method uri (merge {:throw false} opts))]
    (if (= (:status resp) 429)
      (do
        (println "retrying " uri "...")
        (Thread/sleep (:retry-after (:headers resp)))
        (recur method uri opts))
      (if (get #{200 201 202 203 204 205 206 207 300 301 302 303 304 307} (:status resp))
        resp
        (throw (ex-info (str "Exceptional status code: " (:status resp)
                             "\n uri: " uri
                             "\n response: " (with-out-str (pp/pprint resp))) resp))))))

(defn get-retry [uri opts]
  (http-retry http/get uri opts))

(defn post-retry [uri opts]
  (http-retry http/post uri opts))

(defn get-all-pages
  ([link token]
   (get-all-pages link token 0 [] {}))

  ([link token filters]
   (get-all-pages link token 0 [] filters))

  ([link token offset result filters]
   (let [resp (-> (get-retry
                   link
                   {:headers
                    {"Authorization" (str "Bearer " token)}
                    :query-params
                    (merge {"_offset" offset} filters)})
                  (:body)
                  (json/parse-string))
         total (get resp "result_total")]
     (if (> (- total offset) 0)
       (recur link token (+ 100 offset)
              (concat result (get resp "data")) filters)
       result))))

(defn list-subscribed [token]
  (let [mods (get-all-pages
              "https://api.mod.io/v1/me/subscribed"
              token
              {"game_id" bonelab-id})]
    mods))

(comment
  (pp/pprint
   (map
    #(get % "name")
    (list-subscribed token-a))))

(defn to-mb [bytes]
  (/ bytes 1048576))

(defn format-bar [file]
  (str (apply
        str
        (take 40 (str file
                      (str/join (map (fn [x] (char 32)) (range 100))))))
       "|\t[:bar] :percent% eta :remaining"))

(defn download-and-unzip [url file-name path]
  (when url
    (io/copy
     (:body (get-retry url {:as :stream}))
     (io/file (str "./bmm_tmp/" file-name)))
    (if (= "Linux" (System/getProperty "os.name"))
      (sh/sh "unzip" (str "./bmm_tmp/" file-name) "-d" path)
      (sh/sh "powershell" "Expand-Archive" "-LiteralPath"
             (fs/absolutize (str "./bmm_tmp/" file-name))
             "-DestinationPath"
             path))))

(defn render-bar [bar name]
  (str "\r"
       (pr/render
        bar
        {:length 20
         :format (format-bar name)
         :complete \#})))

(defn download-mod-list [mods token path threads]
  (when-not (fs/exists? ( bmm-db-path))
    (fs/create-file ( bmm-db-path))
    (-> (fs/file ( bmm-db-path))
        (fs/write-lines ["{}"])))
  (let [download (chan threads)
        done (chan (count mods))
        finished (chan)
        mod-progress (atom {})
        total-progress (atom nil)
        bmm-db (-> (fs/read-all-lines ( bmm-db-path))
                   (#(apply str %))
                   (json/parse-string))
        mods (->> (sort-by #(get-in % ["modfile" "filesize"]) mods)
                  (filter
                   (fn [{id "id" {version "version"} "modfile" :as mod}]
                     (let [{disk-version "version" :as disk-mod} (get bmm-db (str id))]
                       (not= version disk-version)))))
        bmm-db (reduce (fn [db {id "id" {version "version"} "modfile"}]
                         (assoc db id {"version" version}))
                       bmm-db
                       mods)]

    (println "Updating" (count mods) "mods")
    (reset! total-progress (pr/progress-bar (count mods)))
    (when (fs/exists? "./bmm_tmp/")
      (fs/delete-tree (fs/file "./bmm_tmp/")))
    (fs/create-dir "./bmm_tmp/")
    ;; recieve and download
    (dotimes [_ threads]
      (go-loop []
        (let [[url file-name name] (<! download)]
          (when url
            (<!
             (async/thread (download-and-unzip url file-name path)))
            (>! done name)
            (recur)))))
    ;; recieve from above go loop and then print output
    (go
      (dotimes [n (count mods)]
        (<! done)
        (swap! total-progress assoc :progress n))
      (>! finished true))
    ;; watch files changing
    (start-watch
     [{:path "./bmm_tmp/"
       :event-types [:create :modify :delete]
       :callback
       (fn [event filename]
         (swap! mod-progress assoc-in
                [(fs/file-name filename) :progress] (to-mb (fs/size filename))))}])

    (go
      (while (= 0 (count @mod-progress))
        (<! (async/thread (Thread/sleep 500))))
      (while (not= 0 (count @mod-progress)) []
             (<!
              (async/thread (Thread/sleep 500)
                            (print (str (char 27) "[2J")) ;; clear screen
                            (print (str (char 27) "[H")) ;; move cursor to 0,0
                            (doseq [[file bar] @mod-progress]
                              (when (and (not= (:total bar) (:progress bar))
                                         (not= 0 (:progress bar)))
                                (print (str (render-bar bar file) "\n"))))
                            (print (str "\r" (pr/render @total-progress)))
                            (flush)))))

    (doseq [mod mods]
      (let [id (get mod "id")
            modfile (->
                     (get-all-pages
                      (str "https://api.mod.io/v1/games/"
                           bonelab-id "/mods/" id "/files") token) ;; get modfiles
                     ((fn [modfiles] (filter ;; filter only windows
                                      #(= (get (first (get % "platforms"))
                                               "platform")
                                          "windows")
                                      modfiles)))
                     ((fn [modfiles] (sort-by #(get % "date_updated") modfiles)))
                     (reverse)
                     (first)) ;; sort by newest
            mod-size (get modfile "filesize")
            url (get-in modfile ["download" "binary_url"])
            file-name (get-in mod ["modfile" "filename"])
            name (get mod "name")]
        (swap! mod-progress assoc file-name (pr/progress-bar (to-mb mod-size)))
        (>!! download [url file-name name])))
    (<!! finished)
    (-> (fs/file ( bmm-db-path))
        (fs/write-lines [(json/generate-string bmm-db {:pretty true})]))
    (println "Deleting tmp files...")
    (fs/delete-tree (fs/file "./bmm_tmp/"))))

(defn search-mod [name author token]
  (first
   (reverse
    (sort-by #(fuzzy/dice name (get % "name"))
             (get-all-pages (str "https://api.mod.io/v1/games/" bonelab-id "/mods")
                            token
                            {"submitted_by_display_name" author})))))
;; usage
(comment
  (let [mod
        (search-mod "BaBa's ToyBox" "BaBaCorp" token)]
    [(get-in
      mod
      ["submitted_by"
       "username"])
     (get mod "name")]))

(defn subscribe-installed
  [token mod-path]
  (println "warning!!! this will likely take a long time")
  (when-not (fs/exists? ( bmm-db-path))
    (fs/create-file ( bmm-db-path))
    (-> (fs/file ( bmm-db-path))
        (fs/write-lines ["{}"])))
  (let [manifests (filter
                   #(str/includes? % ".manifest")
                   (fs/list-dir mod-path))
        bmm-db (-> (fs/read-all-lines ( bmm-db-path ))
                   (#(apply str %))
                   (json/parse-string))
        pallets (filter
                 #(not (str/includes? % "/SLZ."))
                 (map
                  (fn [file]
                    (->
                     (fs/read-all-lines file)
                     (#(apply str %))
                     (json/parse-string) ;; read manifest json
                     (get-in ["objects"
                              "1"
                              "palletPath"])))
                  manifests))
        mod-titles (map
                    (fn [file]
                      (->
                       (fs/read-all-lines
                        (str/replace
                         (str mod-path
                              (second
                               (str/split file
                                          #"/AppData/LocalLow/Stress Level Zero/BONELAB\\Mods")))
                         "\\" "/"))
                       (#(apply str %))
                       (json/parse-string) ;; read pallete json
                       (get-in ["objects" "1"])
                       ((fn [obj] [(get obj "title") (get obj "author")]))))
                    pallets)
        mods (->> (map (fn [[title author]]
                         (println "searching" title author)
                         (-> (search-mod title author token)
                             ((fn [mod]
                                (println "found" (get mod "name"))
                                mod))))
                       mod-titles)
                  (filter (fn [{id "id"}]
                            (nil? (get bmm-db id)))))]
    (println "Number of mods being subscribed: " (count mods))
    (doseq [[idx mod] (map-indexed (fn [idx mod] [idx mod]) mods)]
      (println (str idx "/" (count mods)) "Subscribing to " (get mod "name"))
      (post-retry
       (str "https://api.mod.io/v1/games/"
            bonelab-id "/mods/"
            (get mod "id")
            "/subscribe")
       {:headers
        {"Authorization" (str "Bearer " token)
         "Content-Type" "application/x-www-form-urlencoded"
         "Accept" "application/json"}}))))

(defn -main [& args]
  (try (let [[opts token mod-path] (parse-args args)]
           (def token-a token)
           (when (:subscribe opts)
             (subscribe-installed token mod-path))
           (when (:update opts)
             (download-mod-list
              (list-subscribed token) token mod-path
              (if (:threads opts) (:threads opts) 4)))
           (when (:progress opts)
             (println "starting")
             (print "\n")
             (loop [bar (pr/progress-bar 100)
                    bar2 (pr/progress-bar 1000000000)]
               (Thread/sleep 100)
               (print (str (char 27) "M"))
               (print (str "\r" (pr/render bar) "\n"))
               (print (str "\r" (pr/render bar2)))
               (flush)
               (recur (pr/tick bar) (pr/tick bar2 5000))))

           (println "Finished all tasks!"))
       (catch clojure.lang.ExceptionInfo e
         (println
          (ex-message e)))))
