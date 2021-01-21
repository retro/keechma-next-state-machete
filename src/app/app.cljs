(ns app.app
  (:require ["react-dom" :as rdom]
            [app.controllers.editor]))

(def app
  {:keechma.subscriptions/batcher rdom/unstable_batchedUpdates,
   :keechma/controllers {:editor #:keechma.controller{:params true}}})