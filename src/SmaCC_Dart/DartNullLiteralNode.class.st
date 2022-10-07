Class {
	#name : #DartNullLiteralNode,
	#superclass : #DartProgramNode,
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartNullLiteralNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitNullLiteral: self
]
