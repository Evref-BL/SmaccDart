Class {
	#name : #SmaCCDefinitionHighlighter,
	#superclass : #Object,
	#traits : 'TSmaCCDefinitionNodeVisitor',
	#classTraits : 'TSmaCCDefinitionNodeVisitor classTrait',
	#instVars : [
		'text',
		'theme'
	],
	#category : #'Smacc_Dart_Extension'
}

{ #category : #initialization }
SmaCCDefinitionHighlighter >> highlights: ast [

	text := ast completeSource asString asText.
	ast acceptVisitor: self.
	^ text
]

{ #category : #initialization }
SmaCCDefinitionHighlighter >> initialize [

	super initialize.
	theme := Dictionary new.
	Smalltalk ui theme shStyleTable do: [ :element |
		| styleElement |
		styleElement := SmaCCDefinitionHighlighterStyle new.
		styleElement color: (Color colorFrom: element second).
		"styles size > 1 ifTrue: [
			styleElement emphasis: (TextEmphasis perform: styles second) ]."
		theme at: element first put: styleElement ].
	text := ''
]

{ #category : #generated }
SmaCCDefinitionHighlighter >> visitClassProductionExpression: aClassProductionExpression [

	aClassProductionExpression classname ifNotNil: [ :classNameToken |
		theme at: #self ifPresent: [ :style |
			style color ifNotNil: [ :color |
				text
					addAttribute: (TextColor new color: color)
					from: classNameToken startPosition
					to: classNameToken stopPosition ] ] ].

	^ self visitProductionExpression: aClassProductionExpression
]

{ #category : #generated }
SmaCCDefinitionHighlighter >> visitFlagDirective: aFlagDirective [

	theme at: #pragma ifPresent: [ :style |
		style color ifNotNil: [ :color |
			text
				addAttribute: (TextColor new color: color)
				from: aFlagDirective startPosition
				to: aFlagDirective stopPosition ] ].
	^ self visitDirective: aFlagDirective
]

{ #category : #generated }
SmaCCDefinitionHighlighter >> visitNodeNameDirective: aNodeNameDirective [

	theme at: #pragma ifPresent: [ :style |
		style color ifNotNil: [ :color |
			text
				addAttribute: (TextColor new color: color)
				from: aNodeNameDirective startPosition
				to: aNodeNameDirective stopPosition ] ].
	^ self visitDirective: aNodeNameDirective
]

{ #category : #generated }
SmaCCDefinitionHighlighter >> visitPostfixTerm: aPostfixTerm [

	theme at: #pragma ifPresent: [ :style |
		style color ifNotNil: [ :color |
			text
				addAttribute: (TextColor new color: color)
				from: aPostfixTerm stopPosition
				to: aPostfixTerm stopPosition ] ].

	^ self visitDefinition: aPostfixTerm
]

{ #category : #generated }
SmaCCDefinitionHighlighter >> visitProductionSymbol: aProductionSymbol [

	theme at: #instVar ifPresent: [ :style |
		style color ifNotNil: [ :color |
			text
				addAttribute: (TextColor new color: color)
				from: aProductionSymbol name startPosition
				to: aProductionSymbol name stopPosition ] ].

	aProductionSymbol variableNameToken ifNotNil: [ :variableNameToken |
		theme at: #string ifPresent: [ :style |
			style color ifNotNil: [ :color |
				text
					addAttribute: (TextColor new color: color)
					from: variableNameToken startPosition
					to: variableNameToken stopPosition ] ] ].

	^ self visitSymbol: aProductionSymbol
]

{ #category : #generated }
SmaCCDefinitionHighlighter >> visitTokenSymbol: aTokenSymbol [

	theme at: #argument ifPresent: [ :style |
		style color ifNotNil: [ :color |
			text
				addAttribute: (TextColor new color: color)
				from: aTokenSymbol name startPosition
				to: aTokenSymbol name stopPosition ] ].

	aTokenSymbol variableNameToken ifNotNil: [ :variableNameToken |
		theme at: #string ifPresent: [ :style |
			style color ifNotNil: [ :color |
				text
					addAttribute: (TextColor new color: color)
					from: variableNameToken startPosition
					to: variableNameToken stopPosition ] ] ].

	^ self visitSymbol: aTokenSymbol
]
