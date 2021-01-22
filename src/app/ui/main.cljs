(ns app.ui.main
  (:require [keechma.next.helix.core :refer [with-keechma use-sub dispatch]]
            [keechma.next.helix.lib :refer [defnc]]
            [helix.core :as hx :refer [$ <>]]
            [helix.dom :as d]
            [helix.hooks :as hooks]
            [oops.core :refer [ocall oset! oget oget+]]
            [com.verybigthings.state-machete.core :refer [in-state?]]
            ["react-use-dimensions" :as useDimensions]
            [com.verybigthings.state-machete.core :as sm]))

(defn make-dispatch
  ([props event] (make-dispatch props event {}))
  ([props event payload]
   (fn [e]
     (ocall e :preventDefault)
     (ocall e :stopPropagation)
     (dispatch props :editor event (merge payload {:native-event e})))))

(defn client-bounding-rect->clj [data]
  (reduce
    (fn [acc prop]
      (assoc acc prop (oget+ data (str "?" (name prop)))))
    {}
    [:top :right :bottom :left :width :height :x :y]))

(defnc Selector [props]
  (let [editor (use-sub props :editor)
        editor-data (:fsm/data editor)
        {:keys [start-x start-y current-x current-y]} (:app.controllers.editor/selecting-coords editor-data)
        select-mode (:select-mode editor-data)
        top (min start-y current-y)
        height (- (max start-y current-y) top)
        left (min start-x current-x)
        width (- (max start-x current-x) left)]
    (when (and current-x current-y)
      (d/div
        {:class ["absolute border bg-opacity-20 pointer-events-none"
                 (if (= :full select-mode)
                   "border-blue-400 bg-blue-400"
                   "border-green-400 bg-green-400")]
         :style {:top (str top "px")
                 :left (str left "px")
                 :width (str width "px")
                 :height (str height "px")}}))))

(defnc Selected [props]
  (let [editor (use-sub props :editor)
        editor-data (:fsm/data editor)
        selected (:selected editor-data)
        elements (get-in editor-data [:store :elements])]
    (when (< 1 (count selected))
      (let [{:keys [x1 x2 y1 y2]} (reduce
                                    (fn [acc el-id]
                                      (let [el (get elements el-id)
                                            {:keys [top left width height]} el
                                            x2 (+ left width)
                                            y2 (+ top height)]
                                        (cond-> acc
                                          (< top (:y1 acc)) (assoc :y1 top)
                                          (< left (:x1 acc)) (assoc :x1 left)
                                          (> x2 (:x2 acc)) (assoc :x2 x2)
                                          (> y2 (:y2 acc)) (assoc :y2 y2))))
                                    {:x1 ##Inf :x2 ##-Inf :y1 ##Inf :y2 ##-Inf}
                                    selected)]
        (d/div
          {:class "absolute"
           :onMouseDown (make-dispatch props :selected-mousedown)
           :onMouseUp (make-dispatch props :selected-mouseup)
           :onMouseLeave (make-dispatch props :selected-mouseleave)
           :onMouseMove (make-dispatch props :selected-mousemove)
           :style {:top (str y1 "px")
                   :left (str x1 "px")
                   :width (str (- x2 x1) "px")
                   :height (str (- y2 y1) "px")}})))))

(defnc SelectedBorder [props]
  (let [editor (use-sub props :editor)
        editor-data (:fsm/data editor)
        selected (:selected editor-data)
        elements (get-in editor-data [:store :elements])]
    (when (< 1 (count selected))
      (let [{:keys [x1 x2 y1 y2]} (reduce
                                    (fn [acc el-id]
                                      (let [el (get elements el-id)
                                            {:keys [top left width height]} el
                                            x2 (+ left width)
                                            y2 (+ top height)]
                                        (cond-> acc
                                          (< top (:y1 acc)) (assoc :y1 top)
                                          (< left (:x1 acc)) (assoc :x1 left)
                                          (> x2 (:x2 acc)) (assoc :x2 x2)
                                          (> y2 (:y2 acc)) (assoc :y2 y2))))
                                    {:x1 ##Inf :x2 ##-Inf :y1 ##Inf :y2 ##-Inf}
                                    selected)]
        (d/div
          {:class "absolute border border-blue-400 pointer-events-none"
           :style {:top (str y1 "px")
                   :left (str x1 "px")
                   :width (str (- x2 x1) "px")
                   :height (str (- y2 y1) "px")}})))))

(defnc MainRenderer [props]
  (let [editor (use-sub props :editor)
        editor-data (:fsm/data editor)
        editor-state (:fsm/state editor)
        scroll-x (:scroll/x editor-data)
        scroll-y (:scroll/y editor-data)
        store (:store editor-data)
        selected (:selected editor-data)
        {:keys [width height]} editor-data
        [editor-wrap-ref editor-wrap-size editor-el] (useDimensions)]
    (hooks/use-effect
      [scroll-x scroll-y width height editor-el]
      (when editor-el
        (oset! editor-el :scrollLeft scroll-x)
        (oset! editor-el :scrollTop scroll-y)))
    (hooks/use-effect
      [editor-wrap-size]
      (dispatch props :editor :measure-element (client-bounding-rect->clj editor-wrap-size)))
    ;;(println editor-data)
    ;;(println editor-state)
    (d/div {:class "w-screen h-screen flex items-center justify-center"}
      #_(d/div {:class "absolute border border-black bottom-0 right-0 mb-3 mr-3"}
        (with-out-str (cljs.pprint/pprint editor-data)))
      (d/div {:class ["border border-black overflow-scroll w-screen h-screen relative"
                      (when (in-state? editor :moving-canvas) "cursor-move")]
              :id "editor-wrap"
              :ref editor-wrap-ref
              :onMouseEnter (make-dispatch props :canvas-mouseenter)
              :onMouseLeave (make-dispatch props :canvas-mouseleave)
              :onMouseMove (make-dispatch props :canvas-mousemove)
              :onMouseDown (make-dispatch props :canvas-mousedown)
              :onMouseUp (make-dispatch props :canvas-mouseup)
              :onClick (make-dispatch props :canvas-click)}
        (d/div {:style {:background "url(/images/grid.png)"
                        :width (str (:width editor-data) "px")
                        :height (str (:height editor-data) "px")}})
        (when (not (sm/in-state? editor :selecting))
          ($ Selected {& props}))
        (map
          (fn [el-id]
            (let [el (get-in store [:elements el-id])]
              (d/div
                {:key el-id
                 :class ["absolute" (when (contains? selected el-id) "border border-blue-400")]
                 :onMouseEnter (make-dispatch props :element-mouseenter {:id el-id})
                 :onMouseLeave (make-dispatch props :element-mouseleave {:id el-id})
                 :onMouseMove (make-dispatch props :element-mousemove {:id el-id})
                 :onMouseDown (make-dispatch props :element-mousedown {:id el-id})
                 :onMouseUp (make-dispatch props :element-mouseup {:id el-id})
                 :onClick (make-dispatch props :element-click {:id el-id})
                 :style {:width (str (:width el) "px")
                         :height (str (:height el) "px")
                         :top (str (:top el) "px")
                         :left (str (:left el) "px")
                         :background (:background el)}})))
          (:order store))
        (map
          (fn [el-id]
            (let [el (get-in store [:elements el-id])]
              (d/div
                {:key el-id
                 :class ["absolute" (when (contains? selected el-id) "border border-blue-400 pointer-events-none")]
                 :onMouseEnter (make-dispatch props :element-mouseenter {:id el-id})
                 :onMouseLeave (make-dispatch props :element-mouseleave {:id el-id})
                 :onMouseMove (make-dispatch props :element-mousemove {:id el-id})
                 :onMouseDown (make-dispatch props :element-mousedown {:id el-id})
                 :onMouseUp (make-dispatch props :element-mouseup {:id el-id})
                 :onClick (make-dispatch props :element-click {:id el-id})
                 :style {:width (str (:width el) "px")
                         :height (str (:height el) "px")
                         :top (str (:top el) "px")
                         :left (str (:left el) "px")}})))
          selected)
        (when (not (sm/in-state? editor :selecting))
          ($ SelectedBorder {& props}))
        (when (sm/in-state? editor :selecting)
          (<>
            (d/div {:class "absolute top-0 left-0"
                    :style {:width (str (:width editor-data) "px")
                            :height (str (:height editor-data) "px")}})
            ($ Selector {& props})))))))

(def Main (with-keechma MainRenderer))