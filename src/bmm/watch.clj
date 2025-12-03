(set! *warn-on-reflection* true)
(ns bmm.watch
  (:require [clojure.java.io :as io]
            [clojure.core.async
             :as async
             :refer [<! <!! >! >!! chan go go-loop close!]])

  (:import [java.nio.file FileSystem FileSystems Paths Path StandardWatchEventKinds
            WatchService WatchKey WatchEvent]
           [java.net URI]
           [java.nio.file.attribute BasicFileAttributes]))

(defn start-watch-service
  "
  dir-path is the path
  on-event is (f event-type file) that runs anytime a file changes in dir
  "
  [^String dir-path on-event]
  (let [path ^Path (.toPath (io/file dir-path))
        watch-service ^WatchService (.newWatchService (FileSystems/getDefault))
        watch-key (.register path watch-service
                            (into-array
                                         [StandardWatchEventKinds/ENTRY_CREATE
                                          StandardWatchEventKinds/ENTRY_DELETE
                                          StandardWatchEventKinds/ENTRY_MODIFY]))]
    (println "Watching directory for changes:" dir-path)

    (async/thread
     (loop []
       (let [key ^WatchKey (.take watch-service)]
         (doseq [event ^WatchEvent (.pollEvents key)]
           (on-event (WatchEvent/.kind event)
                     (str dir-path (-> (WatchEvent/.context event)
                                       (.toString)))))

           ;; (println "Event kind:" (WatchEvent/.kind event)
           ;;          "File:" (WatchEvent/.context event)))
         (.reset key) ; Reset the key to receive further watch events
         (recur))))))

(comment
  (start-watch-service "./bmm_tmp/" (fn [event file] (println file))))
