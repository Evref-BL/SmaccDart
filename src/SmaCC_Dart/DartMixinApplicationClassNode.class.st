Class {
	#name : #DartMixinApplicationClassNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'typeWithParameters',
		'mixinApplication'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartMixinApplicationClassNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitMixinApplicationClass: self
]

{ #category : #generated }
DartMixinApplicationClassNode >> mixinApplication [

	^ mixinApplication
]

{ #category : #generated }
DartMixinApplicationClassNode >> mixinApplication: aDartMixinApplicationNode [

	self mixinApplication notNil ifTrue: [ 
		self mixinApplication parent: nil ].
	mixinApplication := aDartMixinApplicationNode.
	self mixinApplication notNil ifTrue: [ 
		self mixinApplication parent: self ]
]

{ #category : #generated }
DartMixinApplicationClassNode >> nodeVariables [

	^ #( #typeWithParameters #mixinApplication )
]

{ #category : #generated }
DartMixinApplicationClassNode >> typeWithParameters [

	^ typeWithParameters
]

{ #category : #generated }
DartMixinApplicationClassNode >> typeWithParameters: aDartTypeNode [

	self typeWithParameters notNil ifTrue: [ 
		self typeWithParameters parent: nil ].
	typeWithParameters := aDartTypeNode.
	self typeWithParameters notNil ifTrue: [ 
		self typeWithParameters parent: self ]
]
