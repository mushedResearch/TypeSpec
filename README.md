# TypeSpec

## Introduction
Types as information (a perspective)

Data types in software have some advantages over an untyped code and some strong implications for the software result.
   
Types provide meaning, a semantic binding the binary data in memory. A slice of 32 bits doesn't mean anything unless further information is known about how to interpret i.e. int, float, or unsinged int. This "further information" is called metadata because it is data that describes other data. Types are a form of metadata that is written by the programmer and used by the compiler to make strong guarantees about behavior. 

In a compiler/target without type reification like dotnet, the type information is accessible in the program its self. In dotnet the complex type information is accessed via the reflection api. 

Dynamic languages or languages without a good type system, the semantic information has to be embedded into the software or treated as data.

An algebraic type system is a mathematically vigorous way of encoding semantics about other data.

A dependent type system is a type system where some types (metadata) may depend on data.



