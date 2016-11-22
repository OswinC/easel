; ModuleID = 'ttt.c'

@c = common global [960 x [960 x i32]] zeroinitializer, align 16

; Function Attrs: nounwind uwtable
define i32 @main() #0 {
  %1 = call i32 @do_draw(i32* getelementptr inbounds ([960 x [960 x i32]], [960 x [960 x i32]]* @c, i32 0, i32 0, i32 0), i32 960, i32 960, i32 0, i32 0)
  ret i32 0
}

declare i32 @do_draw(i32*, i32, i32, i32, i32) #1

