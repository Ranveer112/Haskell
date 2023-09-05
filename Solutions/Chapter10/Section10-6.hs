--------------------------------------------------------------------------
--                                                                      --
--	Section 10.6: Algebraic types and type classes			--
--                                                                      --
--	(c) Simon Thompson, 1995.					--
--                                                                      --
--------------------------------------------------------------------------

-------------------------------------------------------------------------- 
--	Moveable objects on a two dimemsional grid.			--
-------------------------------------------------------------------------- 

--------------------------------------------------------------------------
--	Moves are specified by vectors:					--
--------------------------------------------------------------------------

data Vector = Vector Float Float

--------------------------------------------------------------------------
--      The permissible moves are a translation (called move), 		--
--	reflections in the X and Y axes and a rotation through		--
--	180 degrees. 							--
--                                                                      --
--	Note that the rotation has a default definition from the 	--
--	two reflection functions.					--
--------------------------------------------------------------------------

class Movable t where
  move      :: Vector -> t -> t
  reflectX  :: t -> t
  reflectY  :: t -> t
  rotate180 :: t -> t
  rotate180 = reflectX . reflectY

--------------------------------------------------------------------------
--	A point is specified by two co-ordinates.			--
--------------------------------------------------------------------------

data Point = Pt Float Float 
	     deriving Text

--------------------------------------------------------------------------
--      Points are the first example of movable objects.		--
--------------------------------------------------------------------------

instance Movable Point where
  move (Vector v1 v2) (Pt c1 c2) = Pt (c1+v1) (c2+v2)
  reflectX (Pt c1 c2)  = Pt c1 (-c2)
  reflectY (Pt c1 c2)  = Pt (-c1) c2
  rotate180 (Pt c1 c2) = Pt (-c1) (-c2)

--------------------------------------------------------------------------
--      Figures are either lines or circles, with the expected 		--
--	components.							--
--                                                                      --
--	Note that Text has to be replaced in Haskell 1.3 by Show.	--
--------------------------------------------------------------------------

data Figure = Line Point Point |
              Circle Point Float 
	      deriving Text

--------------------------------------------------------------------------
--	Figures are movable						--
--------------------------------------------------------------------------

instance Movable Figure where
  move v (Line p1 p2) = Line (move v p1) (move v p2)
  move v (Circle p r) = Circle (move v p) r

  reflectX (Line p1 p2) = Line (reflectX p1) (reflectX p2)
  reflectX (Circle p r) = Circle (reflectX p) r

  reflectY (Line p1 p2) = Line (reflectY p1) (reflectY p2)
  reflectY (Circle p r) = Circle (reflectY p) r

--------------------------------------------------------------------------
--      Lists of movable objects are movable.				--
--------------------------------------------------------------------------

instance Movable t => Movable [t] where
  move v   = map (move v)
  reflectX = map reflectX
  reflectY = map reflectY

--------------------------------------------------------------------------
--	Named objects							--
--------------------------------------------------------------------------

--------------------------------------------------------------------------
--      A type pairing a String with a value.				--
--------------------------------------------------------------------------

data Name t = Name t String

exam1 = Name (Pt 0.0 0.0) "Dweezil"

--------------------------------------------------------------------------
--      Names can be carried along when a function is applied: this is	--
--	called mapName.							--
--------------------------------------------------------------------------

mapName :: (t -> u) -> Name t -> Name u

mapName f (Name obj nm) = Name (f obj) nm

--------------------------------------------------------------------------
--      Naming movable objects keeps them movable.			--
--------------------------------------------------------------------------

instance Movable t => Movable (Name t) where
  move v   = mapName (move v)
  reflectX = mapName reflectX
  reflectY = mapName reflectY

--------------------------------------------------------------------------
--      A class of named types.						--
--------------------------------------------------------------------------

class Named t where
  lookName :: t -> String
  giveName :: String -> t -> t

--------------------------------------------------------------------------
--      First example: (Name t) belongs in the Named class.		--
--------------------------------------------------------------------------


instance Named (Name t) where
  lookName (Name obj nm) = nm
  giveName nm (Name obj _) = (Name obj nm)

--------------------------------------------------------------------------
--      Named and movable objects					--
--------------------------------------------------------------------------

--------------------------------------------------------------------------
--      These are described by the class NamedMovable.			--
--------------------------------------------------------------------------

class (Movable u, Named u) => NamedMovable u

--------------------------------------------------------------------------
--      How to generate instances?					--
--									--
--	We can Name a movable type, or, ...				--
--------------------------------------------------------------------------

instance Movable t => NamedMovable (Name t)

check:: NamedMovable t => t -> t

check = id

--------------------------------------------------------------------------
--      ... pair a Movable type with a Named one.			--
--------------------------------------------------------------------------

instance (Movable u,Named v) => NamedMovable (u,v) 

instance Movable u => Movable (u,v) where
  move v (a,b) = (move v a,b)
  reflectX (a,b) = (reflectX a,b)
  reflectY (a,b) = (reflectY a,b)

instance Named v  => Named (u,v) where
  lookName (a,b) = lookName b
  giveName nm (a,b) = (a, giveName nm b)

