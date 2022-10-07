Extension { #name : #RSLineBuilder }

{ #category : #'*SmaCC_Spec_UI' }
RSLineBuilder >> connectToAllShapes: aBlockOrASymbol [
	"Should this method use connectFrom:ToAll: ?????"
	| tmptoElements |
	self resetCreatedEdges.
	self fromShapes copy do: [ :e | 
		tmptoElements := (aBlockOrASymbol rsValue: e).
		tmptoElements ifNotNil: [ 
			tmptoElements do: [ :toE | 
				self createEdgeIfNecessaryFrom: e to: toE ] ] ].
	self moveBehindIfNecessary: createdEdges.
	^ createdEdges
]
