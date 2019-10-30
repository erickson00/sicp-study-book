# 编译书籍->移除原书籍编译结果->将新编译结果移动到指定目录
gitbook build && rm -rf docs && mv _book docs
