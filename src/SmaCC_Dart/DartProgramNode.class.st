Class {
	#name : #DartProgramNode,
	#superclass : #SmaCCParseNode,
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartProgramNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitProgram: self
]
