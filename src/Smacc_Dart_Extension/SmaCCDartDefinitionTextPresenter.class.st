Class {
	#name : #SmaCCDartDefinitionTextPresenter,
	#superclass : #SpTextPresenter,
	#category : #'Smacc_Dart_Extension'
}

{ #category : #initialization }
SmaCCDartDefinitionTextPresenter >> highlights: source [

	| highlighter |
	highlighter := SmaCCDefinitionHighlighter new.
	^ highlighter highlights:
		  (SmaCCDefinitionParser parseWithErrors: source)
]

{ #category : #api }
SmaCCDartDefinitionTextPresenter >> text: aString [
	"Set the text of the text presenter"

	text := self highlights: aString. 
	adapter widgetDo: [ :w | w textArea setTextWith: text ]
]
