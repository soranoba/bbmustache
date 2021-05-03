# Benchmarks
[benchmark script](bench.escript)

|Library|Time  |
|:------|:-----|
|bbmustache | 39219 |
|mustache.erl | 677911 |

# Check the reference implementation
:warning: For libraries other than bbmustache, there is a possibility that there is a miss.


## comments
https://github.com/mustache/spec/tree/v1.2.1/specs/comments.yml

|    |bbmustache|mustache.erl|
|:---|:------------|:------------|
|Inline|:white_check_mark:|:white_check_mark:|
|Multiline|:white_check_mark:|:white_check_mark:|
|Standalone|:white_check_mark:||
|Indented Standalone|:white_check_mark:||
|Standalone Line Endings|:white_check_mark:||
|Standalone Without Previous Line|:white_check_mark:||
|Standalone Without Newline|:white_check_mark:||
|Multiline Standalone|:white_check_mark:||
|Indented Multiline Standalone|:white_check_mark:||
|Indented Inline|:white_check_mark:|:white_check_mark:|
|Surrounding Whitespace|:white_check_mark:|:white_check_mark:|


## delimiters
https://github.com/mustache/spec/tree/v1.2.1/specs/delimiters.yml

|    |bbmustache|mustache.erl|
|:---|:------------|:------------|
|Pair Behavior|:white_check_mark:||
|Special Characters|:white_check_mark:||
|Sections|:white_check_mark:||
|Inverted Sections|:white_check_mark:||
|Partial Inheritence|:white_check_mark:||
|Post-Partial Behavior|:white_check_mark:||
|Surrounding Whitespace|:white_check_mark:||
|Outlying Whitespace (Inline)|:white_check_mark:||
|Standalone Tag|:white_check_mark:||
|Indented Standalone Tag|:white_check_mark:||
|Standalone Line Endings|:white_check_mark:||
|Standalone Without Previous Line|:white_check_mark:||
|Standalone Without Newline|:white_check_mark:||
|Pair with Padding|:white_check_mark:||


## interpolation
https://github.com/mustache/spec/tree/v1.2.1/specs/interpolation.yml

|    |bbmustache|mustache.erl|
|:---|:------------|:------------|
|No Interpolation|:white_check_mark:|:white_check_mark:|
|Basic Interpolation|:white_check_mark:|:white_check_mark:|
|HTML Escaping|:white_check_mark:||
|Triple Mustache|:white_check_mark:|:white_check_mark:|
|Ampersand|:white_check_mark:|:white_check_mark:|
|Basic Integer Interpolation|:white_check_mark:|:white_check_mark:|
|Triple Mustache Integer Interpolation|:white_check_mark:|:white_check_mark:|
|Ampersand Integer Interpolation|:white_check_mark:|:white_check_mark:|
|Basic Decimal Interpolation|:white_check_mark:|:white_check_mark:|
|Triple Mustache Decimal Interpolation|:white_check_mark:|:white_check_mark:|
|Ampersand Decimal Interpolation|:white_check_mark:|:white_check_mark:|
|Basic Null Interpolation|:white_check_mark:||
|Triple Mustache Null Interpolation|:white_check_mark:||
|Ampersand Null Interpolation|:white_check_mark:||
|Basic Context Miss Interpolation|:white_check_mark:|:white_check_mark:|
|Triple Mustache Context Miss Interpolation|:white_check_mark:|:white_check_mark:|
|Ampersand Context Miss Interpolation|:white_check_mark:|:white_check_mark:|
|Dotted Names - Basic Interpolation|:white_check_mark:||
|Dotted Names - Triple Mustache Interpolation|:white_check_mark:||
|Dotted Names - Ampersand Interpolation|:white_check_mark:||
|Dotted Names - Arbitrary Depth|:white_check_mark:||
|Dotted Names - Broken Chains|:white_check_mark:||
|Dotted Names - Broken Chain Resolution|:white_check_mark:||
|Dotted Names - Initial Resolution|:white_check_mark:||
|Dotted Names - Context Precedence|:white_check_mark:||
|Implicit Iterators - Basic Interpolation|:white_check_mark:||
|Implicit Iterators - HTML Escaping|:white_check_mark:||
|Implicit Iterators - Triple Mustache|:white_check_mark:||
|Implicit Iterators - Ampersand|:white_check_mark:||
|Implicit Iterators - Basic Integer Interpolation|:white_check_mark:||
|Interpolation - Surrounding Whitespace|:white_check_mark:|:white_check_mark:|
|Triple Mustache - Surrounding Whitespace|:white_check_mark:|:white_check_mark:|
|Ampersand - Surrounding Whitespace|:white_check_mark:|:white_check_mark:|
|Interpolation - Standalone|:white_check_mark:|:white_check_mark:|
|Triple Mustache - Standalone|:white_check_mark:|:white_check_mark:|
|Ampersand - Standalone|:white_check_mark:|:white_check_mark:|
|Interpolation With Padding|:white_check_mark:|:white_check_mark:|
|Triple Mustache With Padding|:white_check_mark:|:white_check_mark:|
|Ampersand With Padding|:white_check_mark:|:white_check_mark:|


## inverted
https://github.com/mustache/spec/tree/v1.2.1/specs/inverted.yml

|    |bbmustache|mustache.erl|
|:---|:------------|:------------|
|Falsey|:white_check_mark:|:white_check_mark:|
|Truthy|:white_check_mark:|:white_check_mark:|
|Null is falsey|:white_check_mark:||
|Context|:white_check_mark:|:white_check_mark:|
|List|:white_check_mark:|:white_check_mark:|
|Empty List|:white_check_mark:|:white_check_mark:|
|Doubled|:white_check_mark:|:white_check_mark:|
|Nested (Falsey)|:white_check_mark:||
|Nested (Truthy)|:white_check_mark:||
|Context Misses|:white_check_mark:|:white_check_mark:|
|Dotted Names - Truthy|:white_check_mark:||
|Dotted Names - Falsey|:white_check_mark:||
|Dotted Names - Broken Chains|:white_check_mark:||
|Surrounding Whitespace|:white_check_mark:||
|Internal Whitespace|:white_check_mark:||
|Indented Inline Sections|:white_check_mark:||
|Standalone Lines|:white_check_mark:|:white_check_mark:|
|Standalone Indented Lines|:white_check_mark:||
|Standalone Line Endings|:white_check_mark:||
|Standalone Without Previous Line|:white_check_mark:||
|Standalone Without Newline|:white_check_mark:||
|Padding|:white_check_mark:|:white_check_mark:|


## partials
https://github.com/mustache/spec/tree/v1.2.1/specs/partials.yml

|    |bbmustache|mustache.erl|
|:---|:------------|:------------|
|Basic Behavior|:white_check_mark:||
|Failed Lookup|:white_check_mark:||
|Context|:white_check_mark:||
|Recursion|:white_check_mark:||
|Surrounding Whitespace|:white_check_mark:||
|Inline Indentation|:white_check_mark:||
|Standalone Line Endings|:white_check_mark:||
|Standalone Without Previous Line|:white_check_mark:||
|Standalone Without Newline|:white_check_mark:||
|Standalone Indentation|:white_check_mark:||
|Padding Whitespace|:white_check_mark:||


## sections
https://github.com/mustache/spec/tree/v1.2.1/specs/sections.yml

|    |bbmustache|mustache.erl|
|:---|:------------|:------------|
|Truthy|:white_check_mark:|:white_check_mark:|
|Falsey|:white_check_mark:|:white_check_mark:|
|Null is falsey|:white_check_mark:||
|Context|:white_check_mark:||
|Parent contexts|:white_check_mark:||
|Variable test|||
|List Contexts|:white_check_mark:||
|Deeply Nested Contexts|||
|List|:white_check_mark:||
|Empty List|:white_check_mark:|:white_check_mark:|
|Doubled|:white_check_mark:|:white_check_mark:|
|Nested (Truthy)|:white_check_mark:||
|Nested (Falsey)|:white_check_mark:||
|Context Misses|:white_check_mark:|:white_check_mark:|
|Implicit Iterator - String|:white_check_mark:||
|Implicit Iterator - Integer|:white_check_mark:||
|Implicit Iterator - Decimal|:white_check_mark:||
|Implicit Iterator - Array|:white_check_mark:||
|Dotted Names - Truthy|:white_check_mark:||
|Dotted Names - Falsey|:white_check_mark:||
|Dotted Names - Broken Chains|:white_check_mark:||
|Surrounding Whitespace|:white_check_mark:||
|Internal Whitespace|:white_check_mark:||
|Indented Inline Sections|:white_check_mark:||
|Standalone Lines|:white_check_mark:|:white_check_mark:|
|Indented Standalone Lines|:white_check_mark:||
|Standalone Line Endings|:white_check_mark:||
|Standalone Without Previous Line|:white_check_mark:||
|Standalone Without Newline|:white_check_mark:||
|Padding|:white_check_mark:|:white_check_mark:|


## ~inheritance
https://github.com/mustache/spec/tree/v1.2.1/specs/~inheritance.yml

|    |bbmustache|mustache.erl|
|:---|:------------|:------------|
|Default|||
|Variable|||
|Triple Mustache|||
|Sections|||
|Negative Sections|||
|Mustache Injection|||
|Inherit|||
|Overridden content|||
|Data does not override block|||
|Data does not override block default|||
|Overridden parent|||
|Two overridden parents|||
|Override parent with newlines|||
|Inherit indentation|||
|Only one override|||
|Parent template|||
|Recursion|||
|Multi-level inheritance|||
|Multi-level inheritance, no sub child|||
|Text inside parent|||
|Text inside parent|||


## ~lambdas
https://github.com/mustache/spec/tree/v1.2.1/specs/~lambdas.yml

|    |bbmustache|mustache.erl|
|:---|:------------|:------------|
|Interpolation|||
|Interpolation - Expansion|||
|Interpolation - Alternate Delimiters|||
|Interpolation - Multiple Calls|||
|Escaping|||
|Section|||
|Section - Expansion|||
|Section - Alternate Delimiters|||
|Section - Multiple Calls|||
|Inverted Section|:white_check_mark:|:white_check_mark:|

