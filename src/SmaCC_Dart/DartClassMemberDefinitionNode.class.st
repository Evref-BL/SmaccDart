Class {
	#name : #DartClassMemberDefinitionNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'signature',
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
DartClassMemberDefinitionNode >> nodeVariables [

	^ #( #signature #functionBody #declaration )
]

{ #category : #generated }
DartClassMemberDefinitionNode >> signature [

	^ signature
]

{ #category : #generated }
DartClassMemberDefinitionNode >> signature: aDartProgramNode [

	self signature notNil ifTrue: [ self signature parent: nil ].
	signature := aDartProgramNode.
	self signature notNil ifTrue: [ self signature parent: self ]
]
