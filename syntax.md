
# Types
Unlike C, onyx attaches subtypes like *, [] and () to the type itself instead of the identifier.

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
| T:       | 1          | Cast             | Left-to-right |
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
| ->       | 1          | UFCS operator        | Left-to-right |
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
| \|       | 10         | Bitwise or           | Left-to-right |
| &&       | 11         | Logical and          | Left-to-right |
| \|\|     | 12         | Logical or           | Left-to-right |
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
| \|=      | 14         | Assign by bit or     | Right-to-left |

# Literals
An integer literal: ```123``` or ```123i```

An unsigned integer literal: ```123u``` or ```0x123``` or ```0b101010```

A float literal: ```123.123``` or ```123f``` or ```123.123f```

An array literal: ```T[]{ <INITIALIZERS> }``` or ```T[N]{ <INITIALIZERS> }```

A structure literal: ```T{ <MEMBERS> }```

A string literal: ```"Some UTF-8 text"```
String literals are u8 array views (u8[]).

Anonymous function literal: ```T(<ARGUMENTS>){ <STATEMENTS> }```
Anonymous functions cannot capture local variables.

# Function calls
Onyx uses UFCS (Unified Function Call Syntax),
any function with one argument or more can be called by using the UFCS operator on a
value of the same type as the first argument of the function.
Therefore ```1.7->ceil()->fmod();``` is equal to ```fmod(ceil(1.7));```.

# Variables
A variable is defined either with C syntax:
```isz some_var;```

Or by using the 'let' keyword to automatically deduce the type from the initializer:
```let an_integer = 123;```

'let' cannot be used without an initializer.

Multiple variables can be defined at once like this:
```
f32 a_float, another_float = 1.0, a_third_float;
let an_int = 1, some_float = 123.0, a_bool = false;
```

A compile-time constant can be defined by switching the '=' in the initializer for '::'
```
f32 a_constant :: 1.0;
let another_float_constant :: 2.0;
```

# Functions
### Example function definition
```
let fadd :: f32(f32 x, f32 y) {
	return x + y
}
```

# Ifs
```
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
while 1 {
	do_something();
}
```
### For example
```
for u64 i..100 {
	do_a_thing();
}
```

# Switches
Switches in onyx cannot fall through, and thus they do not affect 'break' statements.

Unhandled cases evoke undefined behaviour if a 'default' case has not been specified, this allows far more aggressive optimization.
```
switch 'A' {
case 'C' {
	do_thing();
}

case 'D', 'E', 'F' {
	do_another_thing();
}

default {
	something();
}
}
```

# Arrays and array views
### Length and data
Both arrays and views have members called ```count``` and ```data```, where count is the number of elements and data is a pointer to the first element.

In static arrays, both of these members are evaluated at compile-time.
For views, they are mutable variables.

### Array views
An array view is any array type that does not have an explicit size (u8[] instead of u8[N]).
Under the hood, this view is simply stored as a "count" and a pointer to the first element.

### Assignment
A major change from C is that onyx allows assignment to static arrays, doing something like:
```
u32[100] arr1, arr2;
arr2 = arr1;
```
Is perfectly valid and copies the contents of arr1 into arr2.

This also works when the source is an array view, although there is no runtime check to guarantee that the memory the view points to is the same size as the destination, so be aware that this can cause out-of-bounds reads if the viewed memory is smaller than necessary.

Assignment to an array view only copies the data pointer and count, not the elements.

### Examples
```
u8[100] arr = " ello ";
u8[] view = arr; // A view pointing to the contents of arr

// Set the first element to 'H', so that arr is now "Hello "
view[0] = 'H';

// Set the last element to '!', arr is now "Hello!"
arr[arr.count - 1] = '!';
```
