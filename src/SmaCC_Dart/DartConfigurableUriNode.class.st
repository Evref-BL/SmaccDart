Class {
	#name : #DartConfigurableUriNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'uri',
		'configurationUris'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartConfigurableUriNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitConfigurableUri: self
]

{ #category : #generated }
DartConfigurableUriNode >> compositeNodeVariables [

	^ #( #configurationUris )
]

{ #category : #generated }
DartConfigurableUriNode >> configurationUris [

	^ configurationUris
]

{ #category : #generated }
DartConfigurableUriNode >> configurationUris: anOrderedCollection [

	self setParents: self configurationUris to: nil.
	configurationUris := anOrderedCollection.
	self setParents: self configurationUris to: self
]

{ #category : #'generated-initialize-release' }
DartConfigurableUriNode >> initialize [
	super initialize.
	configurationUris := OrderedCollection new: 2.
]

{ #category : #generated }
DartConfigurableUriNode >> nodeVariables [

	^ #( #uri )
]

{ #category : #generated }
DartConfigurableUriNode >> uri [

	^ uri
]

{ #category : #generated }
DartConfigurableUriNode >> uri: aDartUriNode [

	self uri notNil ifTrue: [ self uri parent: nil ].
	uri := aDartUriNode.
	self uri notNil ifTrue: [ self uri parent: self ]
]
