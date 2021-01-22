(ns app.controllers.editor
  (:require [com.verybigthings.state-machete.core :as sm]
            [keechma.next.controller :as ctrl]
            [oops.core :refer [ocall oget]]))

(def set-conj (fnil conj #{}))

(derive :editor :keechma/controller)

(defn clamp [val min-val max-val]
  (min max-val (max min-val val)))

(def keymap {32 :space
             91 :cmd})

(defn bind-window-event [event handler]
  (let [handler' (fn [e]
                   (handler {:native-event e}))]
    (ocall js/window :addEventListener (name event) handler')
    #(ocall js/window :removeEventListener (name event) handler')))

(defn space-key? [_ ev]
  (when-let [native-event (:native-event ev)]
    (let [which (oget native-event :?which)]
      (when (= :space (keymap which))
        (ocall native-event :preventDefault)
        true))))

(defn get-el-in-hierarchy [el id]
  (when el
    (let [el-id (oget el :?id)]
      (if (= id el-id)
        el
        (recur (oget el :?parentNode) id)))))

(defn get-next-scroll-position [el scroll-x scroll-y delta-x delta-y]
  (let [max-scroll-x (- (oget el :scrollWidth) (oget el :clientWidth))
        max-scroll-y (- (oget el :scrollHeight) (oget el :clientHeight))]
    {:scroll/x (clamp (+ scroll-x (* -1 delta-x)) 0 max-scroll-x)
     :scroll/y (clamp (+ scroll-y (* -1 delta-y)) 0 max-scroll-y)}))

(defn track-moving-canvas-mousedown-positions [fsm {:keys [native-event]}]
  (if native-event
    (let [wrap-el (ocall js/document :getElementById "editor-wrap")]
      (sm/assoc-in-data fsm [::moving-canvas] {:mouse/x (oget native-event :pageX)
                                               :mouse/y (oget native-event :pageY)
                                               :scroll/y (oget wrap-el :scrollTop)
                                               :scroll/x (oget wrap-el :scrollLeft)}))
    fsm))

(defn dissoc-moving-canvas-mousedown-positions [fsm _]
  (sm/update-data fsm dissoc ::moving-canvas))

(defn update-moving-canvas-mousemove-position [fsm {:keys [native-event]}]
  (let [mouse-x              (oget native-event :pageX)
        mouse-y              (oget native-event :pageY)
        data                 (sm/get-in-data fsm [::moving-canvas])
        diff-x               (- mouse-x (:mouse/x data))
        diff-y               (- mouse-y (:mouse/y data))
        next-scroll-position (get-next-scroll-position (ocall js/document :getElementById "editor-wrap")
                               (:scroll/x data)
                               (:scroll/y data)
                               diff-x
                               diff-y)]
    (sm/update-data fsm merge next-scroll-position)))

(def padding 200)

(defn assoc-element-dimensions [fsm ev]
  (sm/update-data fsm merge {:height (+ (* 2 padding) (:height ev))
                             :width (+ (* 2 padding) (:width ev))
                             :scroll/x padding
                             :scroll/y padding}))

(defn select-elements [fsm]
  (let [{:keys [start-x start-y current-x current-y]} (sm/get-in-data fsm [::selecting-coords])
        elements (vals (sm/get-in-data fsm [:store :elements]))
        [x1 x2] (sort [start-x current-x])
        [y1 y2] (sort [start-y current-y])
        select-mode (if (< start-y current-y) :full :intersect)
        selected (if (= :full select-mode)
                   (reduce
                     (fn [acc el]
                       (let [{:keys [top left width height id]} el]
                         (if (and (<= x1 left (+ left width) x2)
                               (<= y1 top (+ top height) y2))
                           (conj acc id)
                           acc)))
                     #{}
                     elements)
                   (reduce
                     (fn [acc el]
                       (let [{:keys [top left width height id]} el
                             el-y1 top
                             el-y2 (+ top height)
                             el-x1 left
                             el-x2 (+ left width)]
                         (if-not (or (> el-x1 x2)
                                   (< el-x2 x1)
                                   (> el-y1 y2)
                                   (< el-y2 y1))
                           (conj acc id)
                           acc)))
                     #{}
                     elements))]
    (-> fsm
      (sm/assoc-in-data [:selected] selected)
      (sm/assoc-in-data [:select-mode] select-mode))))

(defn mark-dragging-start-coords [fsm]
  (let [selected (sm/get-in-data fsm [:selected])]
    (reduce
      (fn [acc el-id]
        (let [{:keys [top left]} (sm/get-in-data fsm [:store :elements el-id])]
          (-> acc
            (sm/assoc-in-data [:store :elements el-id :drag-start/top] top)
            (sm/assoc-in-data [:store :elements el-id :drag-start/left] left))))
      fsm
      selected)))

(defn move-elements [fsm]
  (let [selected (sm/get-in-data fsm [:selected])
        {:keys [start-x current-x start-y current-y]} (sm/get-in-data fsm [::dragging-coords])
        delta-x (- current-x start-x)
        delta-y (- current-y start-y)]
    (reduce
      (fn [acc el-id]
        (let [{:drag-start/keys [top left]} (sm/get-in-data fsm [:store :elements el-id])]
          (-> acc
            (sm/assoc-in-data [:store :elements el-id :top] (+ delta-y top))
            (sm/assoc-in-data [:store :elements el-id :left] (+ delta-x left)))))
      fsm
      selected)))

(defn make-fsm [ctrl]
  [:fsm/root

   [:fsm/state
    {:fsm.on/exit assoc-element-dimensions}
    [:fsm/transition #:fsm.transition {:event :measure-element
                                       :cond #(and (:width %2) (:height %2))
                                       :target :editor-idle}]]

   [:fsm/parallel

    [:fsm/state
     [:fsm/transition #:fsm.transition {:event :measure-element
                                        :cond #(and (:width %2) (:height %2))
                                        :fsm/on assoc-element-dimensions}]]

    [:fsm/state
     [:fsm/state#editor-idle
      [:fsm/transition #:fsm.transition{:event :canvas-mousemove :target :editor-active}]
      [:fsm/transition #:fsm.transition{:event :canvas-mouseenter :target :editor-active}]]

     [:fsm/state#editor-active
      [:fsm/state#editing
       [:fsm/transition #:fsm.transition{:event :keydown
                                         :target :moving-canvas
                                         :cond space-key?}]
       [:fsm/transition #:fsm.transition {:event :keydown
                                          :cond (fn [fsm {:keys [native-event]}]
                                                  (let [which (oget native-event :?which)]
                                                    (= :cmd (keymap which))))
                                          :target :multiselect}]
       [:fsm/transition #:fsm.transition {:event :element-mousedown
                                          :target :dragging
                                          :fsm/on (fn [fsm {:keys [id native-event]}]
                                                    (let [selected (sm/get-in-data fsm [:selected])]
                                                      (if (contains? selected id)
                                                        fsm
                                                        (sm/assoc-in-data fsm [:selected] #{id}))))}]
       [:fsm/transition #:fsm.transition {:event :selected-mousedown
                                          :target :dragging}]
       [:fsm/transition #:fsm.transition{:event :canvas-mousedown
                                         :target :selecting
                                         :fsm/on (fn [fsm _]
                                                   (sm/assoc-in-data fsm [:selected] #{}))}]]
      [:fsm/state#multiselect
       [:fsm/transition #:fsm.transition {:event :element-click
                                          :fsm/on (fn [fsm {:keys [id native-event]}]
                                                    (let [selected (sm/get-in-data fsm [:selected])]
                                                      (if (contains? selected id)
                                                        (sm/update-in-data fsm [:selected] disj id)
                                                        (sm/update-in-data fsm [:selected] set-conj id))))}]
       [:fsm/transition #:fsm.transition{:event :canvas-mousedown
                                         :target :multiselect-selecting}]
       [:fsm/transition #:fsm.transition {:event :keyup
                                          :target :editing
                                          :cond (fn [fsm {:keys [native-event]}]
                                                  (let [which (oget native-event :?which)]
                                                    (= :cmd (keymap which))))}]
       ]
      [:fsm/state#dragging
       {:fsm.on/enter (fn [fsm {:keys [native-event]}]
                        (-> fsm
                          (mark-dragging-start-coords)
                          (sm/assoc-in-data [::dragging-coords]
                            {:start-x (oget native-event :nativeEvent.pageX)
                             :start-y (oget native-event :nativeEvent.pageY)})))}
       [:fsm/transition #:fsm.transition{:event #{:element-mousemove :selected-mousemove :canvas-mousemove}
                                         :fsm/on (fn [fsm {:keys [native-event]}]
                                                   (-> fsm
                                                     (sm/update-in-data [::dragging-coords] merge
                                                       {:current-x (oget native-event :nativeEvent.pageX)
                                                        :current-y (oget native-event :nativeEvent.pageY)})
                                                     (move-elements)))}]
       [:fsm/transition #:fsm.transition{:event #{:element-mouseup :selected-mouseup :canvas-mouseleave :canvas-mouseup}
                                         :target :multiselect
                                         :cond (fn [fsm {:keys [native-event]}]
                                                 (oget native-event :?metaKey))}]
       [:fsm/transition #:fsm.transition{:event #{:element-mouseup :selected-mouseup :canvas-mouseleave :canvas-mouseup}
                                         :target :editing}]]
      [:fsm/state#selecting
       {:fsm.on/enter (fn [fsm {:keys [native-event]}]
                        (-> fsm
                          (sm/assoc-in-data [::selecting-coords]
                            {:start-x (oget native-event :nativeEvent.offsetX)
                             :start-y (oget native-event :nativeEvent.offsetY)})))
        :fsm.on/exit (fn [fsm _]
                       (sm/update-data fsm dissoc ::selecting-coords))}
       [:fsm/transition
        #:fsm.transition{:event :canvas-mousemove
                         :fsm/on (fn [fsm {:keys [native-event]}]
                                   (-> fsm
                                     (sm/update-in-data [::selecting-coords] merge
                                       {:current-x (oget native-event :nativeEvent.offsetX)
                                        :current-y (oget native-event :nativeEvent.offsetY)})
                                     (select-elements)))}]
       [:fsm/transition #:fsm.transition{:event :canvas-mouseup
                                         :target :editing}]]
      [:fsm/state#moving-canvas
       [:fsm/state {:fsm/id :moving-canvas-mouse}
        [:fsm/state {:fsm/id :moving-canvas.mouse/up}
         [:fsm/transition #:fsm.transition{:event #{:canvas-mousedown :element-mousedown :selected-mousedown}
                                           :target :moving-canvas.mouse/down}]]
        [:fsm/state
         {:fsm/id :moving-canvas.mouse/down
          :fsm.on/enter track-moving-canvas-mousedown-positions
          :fsm.on/exit dissoc-moving-canvas-mousedown-positions}
         [:fsm/transition #:fsm.transition{:event #{:canvas-mouseup :element-mouseup :selected-mouseup}
                                           :target :moving-canvas.mouse/up}]
         [:fsm/transition
          {:fsm.transition/event #{:canvas-mousemove :element-mousemove :selected-mousemove}
           :fsm/on update-moving-canvas-mousemove-position}]]]
       [:fsm/transition #:fsm.transition{:event :keyup
                                         :target :editing
                                         :cond space-key?}]]
      [:fsm/transition #:fsm.transition{:event :canvas-mouseleave :target :editor-idle}]]]]
   [:fsm/transition #:fsm.transition{:event :keydown :cond space-key?}]])

(defmethod ctrl/init :editor [ctrl]
  (let [unbinds [(bind-window-event :keydown #(ctrl/handle ctrl :keydown %))
                 (bind-window-event :keyup #(ctrl/handle ctrl :keyup %))]]
    (assoc ctrl ::unbinds unbinds)))

(defmethod ctrl/start :editor [ctrl _ _ _]
  (-> (make-fsm ctrl)
    sm/compile
    (sm/start
      {:store {:elements {1 {:id 1 :width 200 :height 200 :background "#e4f030" :top 300 :left 500}
                          2 {:id 2 :width 200 :height 200 :background "#ffc09c" :top 300 :left 800}
                          3 {:id 3 :width 200 :height 200 :background "#8feba8" :top 600 :left 500}
                          4 {:id 4 :width 200 :height 200 :background "#bbf0f0" :top 600 :left 800}}
               :order [1 2 3 4]}})))

(defmethod ctrl/handle :editor [{:keys [state*]} cmd payload]
  (let [payload' (if (map? payload) (assoc payload :fsm/event cmd) {:fsm/event cmd :payload payload})]
    ;; (println cmd)
    ;; (println (:fsm/event payload'))
    (swap! state* sm/trigger payload')
    (println (sm/get-active-states @state*))))

(defmethod ctrl/terminate :editor [ctrl]
  (doseq [u (::unbinds ctrl)]
    (u)))