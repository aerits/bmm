(ns main
  (:require [babashka.http-client :as http]
            [babashka.fs :as fs]
            [babashka.cli :as cli]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.pprint :as pp]
            [clojure.core.async
             :as async
             :refer [>! <! >!! <!! go go-loop chan buffer close! thread
                     alts! alts!! timeout]])
  (:gen-class))

(defn show-help
  [spec]
  (cli/format-opts (merge spec {:order (vec (keys (:spec spec)))})))

(def api-key-path (str (fs/home) "/.bmm_token"))
(def bonelab-path (str (fs/home) "/.bmm_bone_path"))
(def bmm-db-path (str (fs/home) "/.bmm_db.json"))
(def bonelab-id 3809)

(def cli-spec
  {:spec
   {:help {:alias :h
           :desc "Show help"}
    :token {:desc "Mod.io OAuth 2 Token"}
    :path {:alias :p
           :desc "Bonelab mod folder path"}}})

(defn parse-args "returns [opts token]" [args]
  (let [opts (cli/parse-opts args cli-spec)]
    (when (:help opts)
      (println (show-help cli-spec))
      (throw (ex-info "" {})))

    (when (:token opts)
      (println "saving user data...")
      (fs/create-file api-key-path)
      (-> (fs/file api-key-path)
          (fs/write-lines [(:token opts)])))

    (when (:path opts)
      (println "saving user data...")
      (fs/create-file bonelab-path)
      (-> (fs/file bonelab-path)
          (fs/write-lines [(:path opts)])))

    (when-not (fs/exists? api-key-path)
      (throw (ex-info
              "Missing OAuth 2 access token.
Rerun the program with --token.
This will save it for future runs" {})))

    (when-not (fs/exists? bonelab-path)
      (throw (ex-info
              "Missing bonelab mod folder path.
Rerun the program with --path
This will save it for future runs" {})))

    [opts
     (-> (fs/file api-key-path) (fs/read-all-lines) (first))
     (-> (fs/file bonelab-path) (fs/read-all-lines) (first))]))

(defn get-retry [uri opts]
  (let [resp  (http/get uri opts)]
    (if (= (:status resp) 429)
      (do
        (println "retrying " uri "...")
        (Thread/sleep (:retry-after (:headers resp)))
        (recur uri opts))
      resp)))

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

(defn download-subscribed [mods token path]
  (when-not (fs/exists? bmm-db-path)
    (fs/create-file bmm-db-path)
    (-> (fs/file bmm-db-path)
        (fs/write-lines ["{}"])))
  (let [download (chan 4)
        done (chan)
        bmm-db (-> (fs/read-all-lines bmm-db-path)
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

    (-> (fs/file bmm-db-path)
        (fs/write-lines [(json/generate-string bmm-db {:pretty true})]))
    (println "Updating" (count mods) "mods")
    (go-loop []
      (let [[url file-name name] (<! download)]
        (when url
          (go
            (sh/sh "mkdir" "./bmm_tmp/")
            (sh/sh "wcurl" url "-o" (str "./bmm_tmp/" file-name))
            (sh/sh "unzip" (str "./bmm_tmp/" file-name) "-d" path)
            (>! done name))
          (recur))))

    (doseq [[idx mod]
            (map-indexed (fn [a b] [a b]) (take 1 mods))]
      (println (str "Started " (get mod "name")))
      (let [url (get-in mod ["modfile" "download" "binary_url"])
            file-name (get-in mod ["modfile" "filename"])
            name (get mod "name")]
        (>!! download [url file-name name])))
    (loop [n 0]
      (if (= n (count mods))
        n
        (do
          (println (str (inc n) "/" (count mods)) "finished " (<!! done))
          (recur (inc n)))))
    (println "Deleting tmp files...")
    (go (sh/sh "rm" "-rf" "./bmm_tmp/"))))

(defn -main [& args]
  (try (let [[opts token mod-path] (parse-args args)
             mods (get-all-pages "https://api.mod.io/v1/me/subscribed" token {"game_id" bonelab-id})]
         (when (:sync opts)
           (download-subscribed mods token mod-path)))
       (catch clojure.lang.ExceptionInfo e
         (println (ex-message e)))))
