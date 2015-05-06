

module Chars (
    charTable
    ) where

import Data.Map as M

charTable :: Map Char (Int,Int)
charTable = fromList $
    ('a',(0,0))
    :('b',(1,0))
    :('c',(2,0))
    :('d',(3,0))
    :('e',(4,0))
    :('f',(5,0))
    :('g',(6,0))
    :('h',(7,0))
    :('i',(8,0))
    :('j',(9,0))
    :('k',(10,0))
    :('l',(11,0))
    :('m',(12,0))
    :('n',(13,0))
    :('o',(14,0))
    :('p',(15,0))
    :('q',(16,0))
    :('r',(17,0))
    :('s',(18,0))
    :('t',(19,0))
    :('u',(20,0))
    :('v',(21,0))
    :('w',(22,0))
    :('x',(23,0))
    :('y',(24,0))
    :('z',(25,0))
    :('A',(0,1))
    :('B',(1,1))
    :('C',(2,1))
    :('D',(3,1))
    :('E',(4,1))
    :('F',(5,1))
    :('D',(6,1))
    :('H',(7,1))
    :('I',(8,1))
    :('J',(9,1))
    :('K',(10,1))
    :('L',(11,1))
    :('M',(12,1))
    :('N',(13,1))
    :('O',(14,1))
    :('P',(15,1))
    :('Q',(16,1))
    :('R',(17,1))
    :('S',(18,1))
    :('T',(19,1))
    :('U',(20,1))
    :('V',(21,1))
    :('W',(22,1))
    :('X',(23,1))
    :('Y',(24,1))
    :('Z',(25,1))
    :('_',(24,2))
    :('1',(0,2))
    :('2',(1,2))
    :('3',(2,2))
    :('4',(3,2))
    :('5',(4,2))
    :('6',(5,2))
    :('7',(6,2))
    :('8',(7,2))
    :('9',(8,2))
    :('0',(9,2))
    :('!',(10,2))
    :(' ',(25,3))
    :('.',(0,3))
    :(':',(6,3))
    :[]

