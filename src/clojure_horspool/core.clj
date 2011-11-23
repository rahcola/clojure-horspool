(ns clojure-horspool.core)

(defn- shift
  [^String pattern]
  (let [m (count pattern)]
    (reduce (fn [shift i]
              (assoc shift
                (.codePointAt pattern i)
                (- (- m 1) i)))
            {}
            (range (dec m)))))

(defn match
  [^String text ^String pattern]
  (let [m (count pattern)
        shift (shift pattern)
        last-of-pattern (.codePointAt pattern (dec m))]
    (some (fn [j]
            (if (empty? (remove (fn [i]
                                  (= (.codePointAt pattern i)
                                     (.codePointAt text (+ j i))))
                                (range (- m 2) 0 -1)))
              j
              false))
          (filter (fn [j]
                    (let [t (.codePointAt text (dec (+ j m)))]
                      (= last-of-pattern t)))
                  (take-while (fn [j] (<= (+ j m) (count text)))
                              (iterate (fn [j]
                                         (let [t (.codePointAt text
                                                               (dec (+ j m)))]
                                           (+ j (shift t m))))
                                       0))))))

(def lorem "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam turpis leo, sodales et aliquet ac, volutpat non magna. Pellentesque interdum ipsum nec orci hendrerit ac malesuada justo mattis. Nullam adipiscing, nunc ut luctus ultricies, nisl metus dictum erat, vitae consectetur velit tellus bibendum metus. Integer sodales interdum accumsan. Etiam vulputate consequat urna, vel volutpat mi facilisis porta. Vivamus vestibulum interdum lacus, ut facilisis tortor vulputate vulputate. Nullam dapibus gravida nibh, ac feugiat dui suscipit vitae. Vestibulum viverra, libero ut pharetra vestibulum, neque urna iaculis nunc, non imperdiet neque risus a lacus. Cras ut massa nec libero vulputate laoreet in sed urna. Praesent malesuada tellus ut eros malesuada convallis. Pellentesque pellentesque risus ut neque sodales hendrerit. Pellentesque tristique, lectus nec lacinia tempor, ante nisl vulputate diam, in aliquet mauris eros at diam. Nunc in sagittis lectus. Curabitur volutpat malesuada rhoncus. Quisque vitae dolor metus. Cras rutrum tincidunt lorem, eu vulputate mauris hendrerit vel. Nullam sed nibh nisi. Morbi nec quam at arcu cursus luctus. Morbi congue elit vel sem rhoncus at auctor mauris tincidunt. Nunc tempor malesuada sollicitudin. Vivamus ut eros quis lacus euismod commodo. Mauris eu lorem et metus ornare dapibus. Curabitur posuere blandit lacus, non aliquam nisl interdum et. Duis egestas mi ut lorem semper molestie. Fusce malesuada convallis feugiat. Aliquam a tellus sapien. Cras auctor lacus nunc. Integer tincidunt gravida nibh, et cursus purus semper nec. Aliquam non velit metus. Integer eu lacinia turpis. Pellentesque eget laoreet enim. Donec malesuada augue eu lorem malesuada faucibus. In nec orci eu dui scelerisque aliquam eget eget risus. Sed volutpat auctor justo, sed gravida tellus tincidunt id. Nunc tortor diam, bibendum non lacinia eget, sagittis id tellus. Quisque accumsan malesuada ligula at sodales. Mauris facilisis magna lectus. Nullam ullamcorper odio eget nunc cursus ultrices. Ut nulla dolor, laoreet sed iaculis sodales, pulvinar nec nisl. Nunc vel sagittis neque. Fusce ipsum tellus, imperdiet ac adipiscing tempus, blandit in velit. Phasellus venenatis lacinia est at lobortis. Fusce condimentum lacus in arcu eleifend laoreet. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Nullam dictum urna vitae sapien suscipit congue volutpat lacinia quam. Duis libero tortor, facilisis auctor ornare tempor, dictum ut mi. Aliquam fermentum risus et eros elementum id placerat libero tincidunt. Vivamus nec nisi id lectus ullamcorper imperdiet eu ac est. Nam imperdiet convallis purus, in ultricies augue tincidunt et. Nam neque urna, iaculis nec feugiat et, posuere in dolor. Vivamus felis ligula, egestas quis tempus a, viverra non nulla.")
