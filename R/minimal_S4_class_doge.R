# # I wrote this to help myself debug an issue with keyword args.
make_doge = setClass( Class = "Doge",
                         slots = c( breed = "character",
                                    name = "character",
                                    is_bored = "logical" ),
                         prototype = list( breed = "actually a bear",
                                           name = "Cooper",
                                           is_bored = T ) )
setGeneric("walk", function( object, jog = F ) standardGeneric("walk"))
setMethod("walk",
          valueClass = "Doge",
          signature = signature( object = "Doge" ),
          function( object, jog = F ) {

  if( jog ){ object@is_bored = F}

  return( object )
  } )


puppy = make_doge()
walk(puppy)
walk(puppy, jog = T)
