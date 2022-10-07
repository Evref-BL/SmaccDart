Class {
	#name : #DartBreakStatementNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'idNotFunction',
		'function'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartBreakStatementNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitBreakStatement: self
]

{ #category : #generated }
DartBreakStatementNode >> function [

	^ function
]

{ #category : #generated }
DartBreakStatementNode >> function: aSmaCCToken [

	function := aSmaCCToken
]

{ #category : #generated }
DartBreakStatementNode >> idNotFunction [

	^ idNotFunction
]

{ #category : #generated }
DartBreakStatementNode >> idNotFunction: aSmaCCToken [

	idNotFunction := aSmaCCToken
]

{ #category : #generated }
DartBreakStatementNode >> tokenVariables [

	^ #( #idNotFunction #function )
]
