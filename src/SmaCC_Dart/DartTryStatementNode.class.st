Class {
	#name : #DartTryStatementNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'blocks',
		'catchParts',
		'typeNotVoids',
		'statements'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartTryStatementNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitTryStatement: self
]

{ #category : #generated }
DartTryStatementNode >> blocks [

	^ blocks
]

{ #category : #generated }
DartTryStatementNode >> blocks: anOrderedCollection [

	self setParents: self blocks to: nil.
	blocks := anOrderedCollection.
	self setParents: self blocks to: self
]

{ #category : #generated }
DartTryStatementNode >> catchParts [

	^ catchParts
]

{ #category : #generated }
DartTryStatementNode >> catchParts: anOrderedCollection [

	self setParents: self catchParts to: nil.
	catchParts := anOrderedCollection.
	self setParents: self catchParts to: self
]

{ #category : #generated }
DartTryStatementNode >> compositeNodeVariables [

	^ #( #blocks #catchParts )
]

{ #category : #'generated-initialize-release' }
DartTryStatementNode >> initialize [
	super initialize.
	blocks := OrderedCollection new: 2.
	catchParts := OrderedCollection new: 2.
	typeNotVoids := OrderedCollection new: 2.
]

{ #category : #generated }
DartTryStatementNode >> nodeVariables [

	^ #( #statements )
]

{ #category : #generated }
DartTryStatementNode >> otherVariables [

	^ #( #typeNotVoids )
]

{ #category : #generated }
DartTryStatementNode >> statements [

	^ statements
]

{ #category : #generated }
DartTryStatementNode >> statements: aDartSequentialStatementsNode [

	self statements notNil ifTrue: [ self statements parent: nil ].
	statements := aDartSequentialStatementsNode.
	self statements notNil ifTrue: [ self statements parent: self ]
]

{ #category : #generated }
DartTryStatementNode >> typeNotVoids [

	^ typeNotVoids
]

{ #category : #generated }
DartTryStatementNode >> typeNotVoids: anOrderedCollection [

	self setParent: self typeNotVoids to: nil.
	typeNotVoids := anOrderedCollection.
	self setParent: self typeNotVoids to: self
]
