### 测试用例

```
set A to relationship(a string, b bool, c number);
set A to insert ("a", false, 1), ("b", false, 2) A;
set B to relationship(a string, b bool, c number);
set B to insert ("b", false, 1), ("b", false, 2) B;
(A union B) except B;
A intersect B;
select c = 1 or a = "b" and c <> null from A union B;
project (a, c, a) (A union B);
A cross join B;
set A to add (l bool) (r number) A;
A left outer join B;
A right outer join B;
A natural join B;
select (b[0] = b[1]) from (A cross join B);
delete A;
```



**注意 对关系的每个操作都不会改变它 只会返回一个新的关系**

### 标识符

`若干字母或数字`：标识符ID

`ID`或`ID[NUMBER]`：属性标识符AID

若表中有两个同名属性`a`，`a`和`a[0]`表示第一个属性，`a[1]`表示第二个属性

### 条件表达式CEXP

`!`：取反

`=`：等于

`<>`：不等于

`<`：小于

`>`：大于

`<=`：小于等于

`>=`：大于等于

`and`：与

`or`：或

### 类型TYPE

`string`

`bool`

`number`

### 关系表达式REXP
`ID`：表示ID对应的关系
`relationship(ID TYPE, ...)`：创建关系 其元组为空
`insert (...), (...), ... REXP;`：在关系中插入一个元组 要求元组必须和属性匹配
`select CEXP from REXP`：选择操作 保留REXP中符合条件的元组
`project (AID, ...) REXP`：投影操作 选择REXP中若干属性组成新关系 可以重复选择同一个属性
`add (ID TYPE, ...) (ID TYPE, ...) REXP`：在关系的左边或右边添加若干新属性（对应的元组中值为null）
`REXP union REXP`
`REXP except REXP`
`REXP intersect REXP`
`REXP cross join REXP`
`REXP natural join REXP`
`REXP left outer join REXP`
`REXP right outer join REXP`

### 语句

`REXP`：运算REXP并显示其结果（不改变原来的REXP）
`set ID to REXP`：把REXP的运算结果绑定到ID上
`delete ID`：将ID和其对应的REXP解绑

### 文法

```
<s>: \s*;

Identity: anychar+;

Bool: 'true' | 'false';

Number: float;

String: string;

Null: 'null';

Literal
	: Bool
	| Number
	| String
	| Null
	;

Int: int32;

Var: Identity ( <s> '[' <s> Int <s> ']' )?;

Type
	: 'string'
	| 'bool'
	| 'number'
	;

Attribute = Identity <s> Type;

AttributeList = Attribute ( <s> ',' <s> Attribute)*;

VarList = Var (<s> ',' <s> Var)*;

Tuple = '(' <s> Literal (<s> ',' <s> Literal)* <s> ')';

TupleList = Tuple (<s> ',' <s> Tuple)*;

ConditionT
	: Literal
	| Var
	| "!" <s> Condition
	| "(" <s> Condition <s> ")"

ConditionOrderBiOperator: '=' | '<>' | '<' | '>' | '<=' | '>=';

ConditionLogicalBiOperator: 'and' | 'or';

ConditionOrder: ConditionT <s> (<s> ConditionOrderOperator <s> ConditionT)*;

ConditionAnd: ConditionOrder (<s> 'and' <s> ConditionOrder)*;

ConditionOr: ConditionAnd (<s> 'or' <s> ConditionAnd)*;

Condition: ConditionOr;

ExprT
	: Identity
	| 'relationship' <s> '(' <s> AttributeList <s> ')'
	| 'insert' <s> TupleList <s> Expr
	| 'select' <s> Condition <s> 'from' <s> Expr
	| 'project' <s> '(' VarList ')' <s> Expr
	| 'add' <s> '(' <s> AttributeList? <s> ')' <s> '(' <s> AttributeList? <s> ')' <s> Expr
	| '(' <s> Expr <s> ')'
	;

ExprOperator
	: 'union'
	| 'except'
	| 'intersect'
	| 'cross join'
	| 'left outer join'
	| 'right outer join'
	| 'natural join'
	;

Expr: ExprT <s> (ExprOperator <s> ExprT)*;

Statement
	: 'delete' <s> ID
	| 'set' <s> ID <s> 'to' <s> Expr
	| Expr
	;

Program: <s> Statement <s> (<s> ';' <s> Statement <s>) EOF;
```



