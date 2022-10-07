Class {
	#name : #DartMethodSignatureNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'constructorSignature',
		'initializers',
		'functionSignature',
		'getterSignature',
		'setterSignature'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartMethodSignatureNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitMethodSignature: self
]

{ #category : #generated }
DartMethodSignatureNode >> constructorSignature [

	^ constructorSignature
]

{ #category : #generated }
DartMethodSignatureNode >> constructorSignature: aDartConstructorSignatureNode [

	self constructorSignature notNil ifTrue: [ 
		self constructorSignature parent: nil ].
	constructorSignature := aDartConstructorSignatureNode.
	self constructorSignature notNil ifTrue: [ 
		self constructorSignature parent: self ]
]

{ #category : #generated }
DartMethodSignatureNode >> functionSignature [

	^ functionSignature
]

{ #category : #generated }
DartMethodSignatureNode >> functionSignature: aDartFunctionSignatureNode [

	self functionSignature notNil ifTrue: [ 
		self functionSignature parent: nil ].
	functionSignature := aDartFunctionSignatureNode.
	self functionSignature notNil ifTrue: [ 
		self functionSignature parent: self ]
]

{ #category : #generated }
DartMethodSignatureNode >> getterSignature [

	^ getterSignature
]

{ #category : #generated }
DartMethodSignatureNode >> getterSignature: aDartGetterSignatureNode [

	self getterSignature notNil ifTrue: [ 
		self getterSignature parent: nil ].
	getterSignature := aDartGetterSignatureNode.
	self getterSignature notNil ifTrue: [ 
		self getterSignature parent: self ]
]

{ #category : #generated }
DartMethodSignatureNode >> initializers [

	^ initializers
]

{ #category : #generated }
DartMethodSignatureNode >> initializers: aDartInitializersNode [

	self initializers notNil ifTrue: [ self initializers parent: nil ].
	initializers := aDartInitializersNode.
	self initializers notNil ifTrue: [ self initializers parent: self ]
]

{ #category : #generated }
DartMethodSignatureNode >> nodeVariables [

	^ #( #constructorSignature #initializers #functionSignature
	     #getterSignature #setterSignature )
]

{ #category : #generated }
DartMethodSignatureNode >> setterSignature [

	^ setterSignature
]

{ #category : #generated }
DartMethodSignatureNode >> setterSignature: aDartSetterSignatureNode [

	self setterSignature notNil ifTrue: [ 
		self setterSignature parent: nil ].
	setterSignature := aDartSetterSignatureNode.
	self setterSignature notNil ifTrue: [ 
		self setterSignature parent: self ]
]
