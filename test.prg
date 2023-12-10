(((lambda f
  (lambda x 
    (if (= x 0) 
        0
        (* x (f (* x 2))))))
 (lambda x "hi"))
20)