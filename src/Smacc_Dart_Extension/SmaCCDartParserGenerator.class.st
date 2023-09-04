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
SmaCCDartParserGenerator >> definitionPage [

	definitionText := self instantiate: SmaCCDartDefinitionTextPresenter.
	^ SpNotebookPage title: 'Definition' provider: [ definitionText ]
]
