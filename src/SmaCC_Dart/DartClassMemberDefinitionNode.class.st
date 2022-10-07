Class {
	#name : #DartClassMemberDefinitionNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'methodSignature',
		'functionBody',
		'declaration'
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
DartClassMemberDefinitionNode >> declaration: aDartProgramNode [

	self declaration notNil ifTrue: [ self declaration parent: nil ].
	declaration := aDartProgramNode.
	self declaration notNil ifTrue: [ self declaration parent: self ]
]

{ #category : #generated }
DartClassMemberDefinitionNode >> functionBody [

	^ functionBody
]

{ #category : #generated }
DartClassMemberDefinitionNode >> functionBody: aDartProgramNode [

	self functionBody notNil ifTrue: [ self functionBody parent: nil ].
	functionBody := aDartProgramNode.
	self functionBody notNil ifTrue: [ self functionBody parent: self ]
]

{ #category : #generated }
DartClassMemberDefinitionNode >> methodSignature [

	^ methodSignature
]

{ #category : #generated }
DartClassMemberDefinitionNode >> methodSignature: aDartProgramNode [

	self methodSignature notNil ifTrue: [ 
		self methodSignature parent: nil ].
	methodSignature := aDartProgramNode.
	self methodSignature notNil ifTrue: [ 
		self methodSignature parent: self ]
]

{ #category : #generated }
DartClassMemberDefinitionNode >> nodeVariables [

	^ #( #methodSignature #functionBody #declaration )
]
