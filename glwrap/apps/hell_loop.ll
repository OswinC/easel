; ModuleID = 'app_glwrap.c'
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

; Function Attrs: nounwind uwtable
define i32 @main(i32 %argc, i8** %argv) #0 {
  %1 = alloca i32, align 4
  %2 = alloca i32, align 4
  %3 = alloca i8**, align 8
  %c = alloca [960 x [960 x i32]], align 16
  %x = alloca i32, align 4
  %y = alloca i32, align 4
  store i32 0, i32* %1, align 4
  store i32 %argc, i32* %2, align 4
  store i8** %argv, i8*** %3, align 8
  store i32 0, i32* %x, align 4
  br label %4

; <label>:4                                       ; preds = %36, %0
  %5 = load i32, i32* %x, align 4
  %6 = icmp slt i32 %5, 960
  br i1 %6, label %7, label %39

; <label>:7                                       ; preds = %4
  store i32 0, i32* %y, align 4
  br label %8

; <label>:8                                       ; preds = %32, %7
  %9 = load i32, i32* %y, align 4
  %10 = icmp slt i32 %9, 960
  br i1 %10, label %11, label %35

; <label>:11                                      ; preds = %8
  %12 = load i32, i32* %x, align 4
  %13 = icmp sgt i32 %12, 480
  br i1 %13, label %14, label %24

; <label>:14                                      ; preds = %11
  %15 = load i32, i32* %y, align 4
  %16 = icmp sgt i32 %15, 480
  br i1 %16, label %17, label %24

; <label>:17                                      ; preds = %14
  %18 = load i32, i32* %y, align 4
  %19 = sext i32 %18 to i64
  %20 = load i32, i32* %x, align 4
  %21 = sext i32 %20 to i64
  %22 = getelementptr inbounds [960 x [960 x i32]], [960 x [960 x i32]]* %c, i64 0, i64 %21
  %23 = getelementptr inbounds [960 x i32], [960 x i32]* %22, i64 0, i64 %19
  store i32 -256, i32* %23, align 4
  br label %31

; <label>:24                                      ; preds = %14, %11
  %25 = load i32, i32* %y, align 4
  %26 = sext i32 %25 to i64
  %27 = load i32, i32* %x, align 4
  %28 = sext i32 %27 to i64
  %29 = getelementptr inbounds [960 x [960 x i32]], [960 x [960 x i32]]* %c, i64 0, i64 %28
  %30 = getelementptr inbounds [960 x i32], [960 x i32]* %29, i64 0, i64 %26
  store i32 0, i32* %30, align 4
  br label %31

; <label>:31                                      ; preds = %24, %17
  br label %32

; <label>:32                                      ; preds = %31
  %33 = load i32, i32* %y, align 4
  %34 = add nsw i32 %33, 1
  store i32 %34, i32* %y, align 4
  br label %8

; <label>:35                                      ; preds = %8
  br label %36

; <label>:36                                      ; preds = %35
  %37 = load i32, i32* %x, align 4
  %38 = add nsw i32 %37, 1
  store i32 %38, i32* %x, align 4
  br label %4

; <label>:39                                      ; preds = %4
  %40 = getelementptr inbounds [960 x [960 x i32]], [960 x [960 x i32]]* %c, i32 0, i32 0
  %41 = bitcast [960 x i32]* %40 to i32*
  %42 = call i32 @do_draw(i32* %41, i32 960, i32 960, i32 0, i32 0)
  ret i32 0
}

declare i32 @do_draw(i32*, i32, i32, i32, i32) #1

attributes #0 = { nounwind uwtable "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = !{!"clang version 3.8.0-2ubuntu4 (tags/RELEASE_380/final)"}
