# SmaCC Dart

This is a dart parser made in Pharo.

## Installation

To install it, first create a [BLMoose image](https://gitlab.forge.berger-levrault.com/bl-drit/bl-moose/bl-moose).

Then, in a playground perform:

```st
Metacello new
  repository: 'gitlab://gitlab.forge.berger-levrault.com:bl-drit/bl-moose/parsers/smacc-dart:main/src';
  baseline: 'SmaccDart';
  onConflict: [ :ex | ex useIncoming ];
  onUpgrade: [ :ex | ex useIncoming ];
  onDowngrade: [ :ex | ex useLoaded ];
  load
```

## Usage

To parse a string: 

```st
DartParser parse:  'class A {
  final int myInt;
  final String myString;
}'
```

From a file

```st
DartParser parseFile:  'path/to/the/file.dart'
```

## Developer

You will find the grammar used for the parser in the file `grammar/dart.g`.
To reimport it, use:

```st
definition := ('path\to\grammar\dart.g' asFileReference contents).
grammarCompiler := SmaCCGrammarCompiler new.
grammarCompiler codeGenerator defaultCategory: 'SmaCC_Dart'.
grammarCompiler
	buildDefinition: definition;
	compileInto: 'DartScanner' andParser: 'DartParser'.
```

You can modify the dart.g file to improve the parser.

> Each time you modify the .g file, remember to regenerate before commiting
