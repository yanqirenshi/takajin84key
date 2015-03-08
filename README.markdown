# Takajin84key
>どうせ幻
みんなミゴロシ
未来忘れて人生捨てて
恐怖の暗号可読不能
秘密の暗号やしきたかじん


## Usage
```common-lisp
 CL-USER> (in-package :takajin)
 TAKAJIN>
 (let ((pass (make-password "xyz")))
   (change-password pass "xyz" "abc")
   (change-password pass "abc" "def")
   (change-password pass "def" "ghi")
   (change-password pass "ghi" "jkl")
   (change-password pass "jkl" "mno")
   (validation pass "mno"))

 T
```


## Installation
```common-lisp
(ql:quickload :takajin84key)
```

## Author

* Satoshi Iwasaki (yanqirenshi@gmail.com)

## Copyright

Copyright (c) 2015 Satoshi Iwasaki (yanqirenshi@gmail.com)

## License

Licensed under the LLGPL License.
