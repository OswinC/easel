; ModuleID = 'easel'

@canvas = global [720 x [960 x i32]] zeroinitializer

declare i32 @draw_default(...)

declare i32 @do_draw(i32*, i32, i32, i32, i32, ...)

define i32 @main() {
entry:
  %canvas = load [720 x [960 x i32]]* @canvas
  %do_draw = call i32 (i32*, i32, i32, i32, i32, ...)* @do_draw(i32* getelementptr inbounds ([720 x [960 x i32]]* @canvas, i32 0, i32 0, i32 0), i32 960, i32 720, i32 200, i32 200)
  ret i32 0
}
