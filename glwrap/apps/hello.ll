declare i32 @draw_default() nounwind

; Definition of main function
define i32 @main() {   ; i32()*
    call i32 @draw_default()
    ret i32 0
}

