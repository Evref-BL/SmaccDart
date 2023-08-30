Class {
	#name : #SmaCCDartParserGenerator,
	#superclass : #SmaCCParserGenerator,
	#category : #'Smacc_Dart_Extension'
}

{ #category : #'world menu' }
SmaCCDartParserGenerator class >> menuCommandOn: aBuilder [
	<worldMenu>
	
	(aBuilder item: #'BL. SmaCC Parser Generator')
		parent: #Browsing;
		order: 99;
		help: 'BL. SmaCC Parser Generator Tool';
		action: [ self open ]
]

{ #category : #initialization }
SmaCCDartParserGenerator >> definition: source [

	definitionText text: (self highlights: source)
]

{ #category : #initialization }
SmaCCDartParserGenerator >> highlights: source [

	| highlighter |
	highlighter := SmaCCDefinitionHighlighter new.

	"	colors := { (SmaCCFlagDirectiveNode -> Color blue) } asDictionary.
	sourceText := source asText.
	ast := SmaCCDefinitionParser parse: source.
	ast allNodes do: [ :node |
		colors at: node class ifPresent: [ :color |
			sourceText
				addAttribute: (TextColor new color: color)
				from: node startPosition
				to: node stopPosition ] ].
"
	^ highlighter highlights: (SmaCCDefinitionParser parse: source)
]
