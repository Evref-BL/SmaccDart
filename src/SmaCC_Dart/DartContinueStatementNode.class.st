Class {
	#name : #DartContinueStatementNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'idNotFunction',
		'function'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartContinueStatementNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitContinueStatement: self
]

{ #category : #generated }
DartContinueStatementNode >> function [

	^ function
]

{ #category : #generated }
DartContinueStatementNode >> function: aSmaCCToken [

	function := aSmaCCToken
]

{ #category : #generated }
DartContinueStatementNode >> idNotFunction [

	^ idNotFunction
]

{ #category : #generated }
DartContinueStatementNode >> idNotFunction: aSmaCCToken [

	idNotFunction := aSmaCCToken
]

{ #category : #generated }
DartContinueStatementNode >> tokenVariables [

	^ #( #idNotFunction #function )
]
