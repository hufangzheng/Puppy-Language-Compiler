#Puppy编程语言参考手册

[TOC]

# 词法事项
## 标识符
由字母、数字、下划线组成的序列。不能由数字开头。
保留字：**while, for, to, break, let, in, end, function, var, type, array, if, then, else, do, of, nil**

## 注释
单行注释由//开头。
多行注释由/*开头，并以/*结束。

## 标点符号
**, : ; ( ) [ ] { } . + - * / = <> < <= > >= && || :=**

字符串的表示由<起始，>结束，例如：<string>

# 语法
## 符号说明：
|  符号   |                         说明                          |
| :-----: | :---------------------------------------------------: |
|   foo   |                 细体表示foo为非终结符                 |
| **foo** |                  粗体表示foo为终结符                  |
|   x*    |                  表示x出现0次或多次                   |
|   x+    |                  表示x出现1次或多次                   |
|   [x]   | 表示x出现0次或1次，**[**  **]**符号表示方括号为终结符 |
|   { }   |        表示一组，**{** **}**表示花括号为终结符        |
|   \|    |                      分隔可选项                       |




## 声明
decs -> {dec}
dec -> tydec
dec -> vardec
dec -> fundec

### 数据类型声明
tydec -> **type** type-id **=** ty
ty -> type-id			<!--普通数据类型声明-->

ty -> **{** tyfields **}** 		<!--record类型声明-->

ty -> **array of** type-id	<!-- 数组类型声明-->

tyfield -> **epsilon**
tyfield -> id : type-id {, id : type-id }*

### 变量声明
vardec -> **var** id **:=** exp

### 函数声明
fundec -> **fun** fun-id **(** tyfield **)** **=** exp

## 表达式
exp -> **let** decs **in** expseq
限定变量的作用域，expseq表示一系列的表达式。声明的decs（数据类型、变量或函数）只在expseq范围内有效。支持嵌套作用域，范围更小的本地作用域内的变量可以与外层声明的变量重名，且本地变量会覆盖外层变量。

exp -> lvalue
lvalue -> id
lvalue -> lvalue **.** id
lvalue -> lvalue **[**exp**]**
左值：读取和分配一个值的地址。
根据左值来获取id、Record类型和数组的值。

expseq -> **(** exp {**;** exp}+**)**
一系列表达式，按顺序求值。

exp -> **int**
整型字面量。

exp -> **string**
字符串字面量。

exp -> **-**exp
表达式前加负号表示负数。

exp -> **fun-id** **(** **)**
exp -> **fun-id** **(** exp {, exp} **)**
函数调用，函数的参数可以为空，最多2个参数。

exp -> exp **+** exp
exp -> exp **-** exp
exp -> exp ***** exp
exp -> exp **/** exp
算数运算。

exp -> exp **<** exp
exp -> exp **>** exp
exp -> exp **<=** exp
exp -> exp **>=** exp
exp -> exp **=** exp
exp -> exp **<>** exp
比较运算。1表示真，0表示假。比较record类型时比较的是实例而不是内容。

exp -> exp **&** exp
exp -> exp **|** exp
布尔表达式。



运算符优先级

| 优先级 | 运算符              |
| ------ | ------------------- |
| 1      | -                   |
| 2      | *  /                |
| 3      | +  -                |
| 4      | =  <  <=  >  >=  <> |
| 5      | &  \|               |
| 6      | :=                  |



符号的结合性

| 结合性   | 符号                                          |
| -------- | --------------------------------------------- |
| 左结合   | +  -  *  /                                    |
| 右结合   |                                               |
| 无结合性 | =  <  <=  >  >=  <>  if  then  else  while := |



exp -> typeid **{** **}**

exp -> type-id **{** **id** **=** exp {, id = exp} **}**

record类型表达式，可以为空record类型，或拥有2个数据域的record类型



exp -> type-id **[** exp **]** **of** exp

对第一个和第二个exp求值，并将第一个exp作为数组的下标，获取对应下标的值，将第二个exp的值作为初始值赋值给对应的下标的数组位置。



exp -> exp **:=** exp

赋值



exp -> **if** exp **then** exp **else** exp

if then else 语句



exp -> **if** exp **then** exp

if then语句



exp -> **while** exp **do** exp

while循环语句



exp -> **for** **id** **:=** exp **to** exp **do** exp

for 循环语句



exp -> **break**

break表达式



exp -> **(** expseq **)**

括号表达式