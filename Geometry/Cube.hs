module Geometry.Cube
(volume,
area)
where

import  Cuboid 

area::Float->Float
area side = Cuboid.area side side side

volume::Float->Float
volume side = Cuboid.volume side side side
