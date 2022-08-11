Class {
	#name : #DartClassMemberDefinitionNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'methodSignature',
		'functionBody',
		'declaration',
		'semicolonToken'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartClassMemberDefinitionNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitClassMemberDefinition: self
]

{ #category : #generated }
DartClassMemberDefinitionNode >> declaration [

	^ declaration
]

{ #category : #generated }
DartClassMemberDefinitionNode >> declaration: anOrderedCollection [

	self setParent: self declaration to: nil.
	declaration := anOrderedCollection.
	self setParent: self declaration to: self
]

{ #category : #generated }
DartClassMemberDefinitionNode >> functionBody [

	^ functionBody
]

{ #category : #generated }
DartClassMemberDefinitionNode >> functionBody: aDartFunctionBodyNode [

	self functionBody notNil ifTrue: [ self functionBody parent: nil ].
	functionBody := aDartFunctionBodyNode.
	self functionBody notNil ifTrue: [ self functionBody parent: self ]
]

{ #category : #generated }
DartClassMemberDefinitionNode >> methodSignature [

	^ methodSignature
]

{ #category : #generated }
DartClassMemberDefinitionNode >> methodSignature: anOrderedCollection [

	self setParent: self methodSignature to: nil.
	methodSignature := anOrderedCollection.
	self setParent: self methodSignature to: self
]

{ #category : #generated }
DartClassMemberDefinitionNode >> nodeVariables [

	^ #( #functionBody )
]

{ #category : #generated }
DartClassMemberDefinitionNode >> otherVariables [

	^ #( #methodSignature #declaration )
]

{ #category : #generated }
DartClassMemberDefinitionNode >> semicolonToken [

	^ semicolonToken
]

{ #category : #generated }
DartClassMemberDefinitionNode >> semicolonToken: aSmaCCToken [

	semicolonToken := aSmaCCToken
]

{ #category : #generated }
DartClassMemberDefinitionNode >> tokenVariables [

	^ #( #semicolonToken )
]
