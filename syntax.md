
# Types
| Name  | Description                    |
| :---: | :----------------------------: |
| i8    | 8 bit integer                  |
| i16   | 16 bit integer                 |
| i32   | 32 bit integer                 |
| i64   | 64 bit integer                 |
| isz   | Pointer sized int              |
| u8    | 8 bit unsigned integer         |
| u16   | 16 bit unsigned integer        |
| u32   | 32 bit unsigned integer        |
| u64   | 64 bit unsigned integer        |
| usz   | Pointer sized uint             |
| f32   | 32 bit float                   |
| f64   | 64 bit float                   |
| b8    | 8 bit unsigned integer         |
| T*    | Pointer to T                   |
| void* | Pointer to nothing             |
| T[]   | View of array of T             |
| T[N]  | N element array of T           |
| T(A)  | Pointer to funtion returning T |

# Type conversions
All types must be explicitly cast unless specified otherwise.

Integers (types starting with i or u) are implicitly converted to each other when necessary,
if a binary operator requires operands of the same size,
the smaller type will be promoted to the larger.

Floating point types work in the same way,
implicitly converting between each other and promoting the smaller type when necessary.

Pointer types can be implicitly converted to/from 'void*' types.

Casting is done with the 'arrow' operator

# Expressions
Order of evaluation is undefined.

## Operator precedence
Unary operators always have precedence over binary operators.

### Unary operators
| Operator | Precedence | Description      | Associativity |
| :------: | :--------: | :--------------: | :-----------: |
| ++       | 1          | Suffix increment | Left-to-right |
| --       | 1          | Suffix decrement | Left-to-right |
| ()       | 1          | Function call    | Left-to-right |
| []       | 1          | Array subscript  | Left-to-right |
| .        | 1          | Member access    | Left-to-right |
| ::       | 1          | UFCS operator    | Left-to-right |
| ->(T)    | 1          | Cast             | Left-to-right |
| --       | 2          | Prefix decrement | Right-to-left |
| ++       | 2          | Prefix increment | Right-to-left |
| -        | 2          | Negate           | Right-to-left |
| !        | 2          | Logical not      | Right-to-left |
| ~        | 2          | Bitwise not      | Right-to-left |
| *        | 2          | Dereference      | Right-to-left |
| &        | 2          | Address-of       | Right-to-left |

### Binary operators
| Operator | Precedence | Description          | Associativity |
| :------: | :--------: | :------------------: | :-----------: |
| *        | 3          | Multiply             | Left-to-right |
| /        | 3          | Divide               | Left-to-right |
| %        | 3          | Modulo               | Left-to-right |
| +        | 4          | Add                  | Left-to-right |
| -        | 4          | Subtract             | Left-to-right |
| <<       | 5          | Shift left           | Left-to-right |
| >>       | 5          | Shift right          | Left-to-right |
| <        | 6          | Lesser               | Left-to-right |
| >        | 6          | Greater              | Left-to-right |
| <=       | 6          | Lesser or equal      | Left-to-right |
| >=       | 6          | Greater or equal     | Left-to-right |
| ==       | 7          | Equal                | Left-to-right |
| !=       | 7          | Not equal            | Left-to-right |
| &        | 8          | Bitwise and          | Left-to-right |
| ^        | 9          | Bitwise xor          | Left-to-right |
| |        | 10         | Bitwise or           | Left-to-right |
| &&       | 11         | Logical and          | Left-to-right |
| ||       | 12         | Logical or           | Left-to-right |
| =        | 14         | Assign               | Right-to-left |
| +=       | 14         | Assign sum           | Right-to-left |
| -=       | 14         | Assign difference    | Right-to-left |
| *=       | 14         | Assign product       | Right-to-left |
| /=       | 14         | Assign quotient      | Right-to-left |
| %=       | 14         | Assign remainder     | Right-to-left |
| <<=      | 14         | Assign left shifted  | Right-to-left |
| >>=      | 14         | Assign right shifted | Right-to-left |
| &=       | 14         | Assign by bit and    | Right-to-left |
| ^=       | 14         | Assign by bit xor    | Right-to-left |
| |=       | 14         | Assign by bit or     | Right-to-left |

# Literals
An integer literal: ```123``` or ```123i```

An unsigned integer literal: ```123u``` or ```0x123``` or ```0b101010```

A float literal: ```123.123``` or ```123f``` or ```123.123f```

An array literal: ```T[]{ <INITIALIZERS> }``` or ```T[N]{ <INITIALIZERS> }```

A structure literal: ```T{ <MEMBERS> }```

Anonymous function literal: ```T(<ARGUMENTS>){ <STATEMENTS> }```
Anonymous functions cannot capture local variables.

# Function calls
Onyx uses UFCS (Unified Function Call Syntax),
any function with one argument or more can be called by using the UFCS operator on a
value of the same type as the first argument of the function.
Therefore ```1.7::ceil()::fmod();``` is equal to ```fmod(ceil(1.7));```.

# Variables
A variable is defined either with C syntax:
```int some_var;```

Or by using the 'let' keyword to automatically deduce the type from the initializer:
```let an_integer = 123;```
'let' cannot be used without an initializer.

Multiple variables can be defined at once like this:
```
float a_float, another_float = 1.0, a_third_float;
let an_int = 1, some_float = 123.0, a_bool = false;
```

A compile-time constant can be defined by switching the '=' in the initializer for '::'
```
float a_constant :: 1.0;
let another_float_constant :: 2.0;
```

# Functions
### Example function definition
```
let fadd :: float(float x, float y) {
	return x + y
};
```

# Ifs
```
if 1:
	do_thing();
elif 2:
	do_thing();
else:
	do_thing();

if 1 {
	do_thing();
}
elif 2 {
	do_thing();
}
else {
	do_thing();
}
```

# Loops
### While example
```
while 1:
	do_something();

while 1 {
	do_something();
}
```
### For example
```
for int i = 0; i < 100; ++i:
	do_a_thing();

for int i = 0; i < 100; ++i {
	do_a_thing();
}
```
### Iteration example
```
int i..100:
	do_other_thing();

int i..100 {
	do_other_thing();
}
```

# Switches
Switches in onyx cannot fall through, and thus they do not affect 'break' statements.
Unhandled cases evoke undefined behaviour if a 'default' case has not been specified.
```
switch 'A' {
case 'B':
	do_thing();

case 'C' {
	do_other_thing();
}

case 'D', 'E', 'F':
	do_another_thing();

default {
	something();
}
}
```

