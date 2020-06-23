{-# LANGUAGE TypeApplications #-}

module Archive where

import Control.Arrow
import Data.Char
import Data.List
import Data.Bool
import Data.Bits
import Text.Printf
import qualified Data.Text.Internal.Read as DTIR

main0=interact$show.(`rem`10).foldr1(^).map read.words

main1=interact$(\(n:_:c)->let m=read n::Int;Just[_,_,s]=find(\[f,t,_]->read f<=m&&m<=read t)(words<$>c)in s).lines

main2=interact$(\[a,b]->[a-b,a+b]>>=show).map read.words

main3=interact$concat.map show.(\(x:y:_)->[(x-y),(x+y)]).map read.words

main4 = interact $ (concat . map (\xs -> (case length xs of 1 -> ""; l -> show l) ++ [head xs]) . group) . concat . tail . lines

main5 = interact $ (show . head . map head . filter (not . even . length) . group . sort . filter (/=0) . map read . words)

main6=interact$intercalate ", ".(\[xs,ys]->zipWith (\x y->concat ["(",x,", ",y,")"]) xs ys).map (map show.sort.map (read @Int).words).lines

main7=interact$concat.(>>=uncurry (replicate.read).span isDigit).words

main8=interact(\s->show$length$filter(\z->length z>1)$group$sort$words$last$lines s)

main9=putStrLn=<<pure.last.show<$>((^)<$>(read.pure.last<$>getLine)<*>(read<$>getLine))

h a=length . filter id . zipWith (==) a
g a=map (\b->((h a b * 100)`div`(length a)) > 90)
main10=interact$unlines.map (bool "fail" "pass").(g<$>head<*>drop 2).lines
-- "Your honor, my client is clearly innocent of all charges and should be released immediately\n2\nYour honor, my client is clearly innocent of all charges and should be relaesed immediately\nUour honir, my clisnt is cleerly innosent of all chargrs and shiuld be releesed innedietily"

main11=interact$return.((show=<<[1..])!!).subtract 1.read
main12=interact$pure.(([0..]>>=show)!!).read

main13=interact$head.sortOn length.group.sort.last.lines -- BAD
main14=interact$head.head.filter ((==1).length).group.sort.tail.words
-- 2 4 2 4 1
-- 1

main15=interact$show.popCount.(\c->read c::Int)

main16=interact$p16
p16 s=[last$printf"%b"$(`uncurry`(read&&&(DTIR.hexDigitToInt.last.printf"%08x".(\z->read z::Int))$s))(*)]::String
-- Given a number N, multiply it by the last character of its hexadecimal form, then print the last digit of its binary representation.
-- 142359
-- 1

-- What you want to achieve is getting the s to the end of the expression, so you can remove it, right. So you try to remove everything that is surrounding it. This might not be the best way to write it, but on way to get closer is first removing the []
-- p s=pure.last.printf"%b"$(`uncurry`(read&&&(D.hexDigitToInt.last.printf"%08x".(\z->read z::Int))$s))(*)::String
-- after that, you can remove more parantheses by applying the arguments to uncurry in their normal order
-- p s=pure.last.printf"%b".uncurry(*)$(read&&&(D.hexDigitToInt.last.printf"%08x".(\z->read z::Int))$s)::String
-- and then your basically done.
-- p=pure.last.printf"%b".uncurry(*).(read&&&(D.hexDigitToInt.last.printf"%08x".(\z->read z::Int)))

main17=interact$p17
p17 s=r
 where
  l=lines s
  x=words<$>(init$tail$l)
  t=last$l
  r=show$(x,t)

-- p "4\nShanghai 25\nMadrid 6\nBangkok 10\nManila 13\nMa"
-- 19

main18=interact$show.c18.read
c18 n=any(\i->i^i+i==n)[1..n]

c18_2 n=or[i^i+i==n|i<-[1..n]]

-- 6
-- True

main19=interact$p19
p19 s=show$product$sum.(digitToInt<$>)<$>words s
p19_2=show.product.map(sum.map digitToInt).words

-- Given a string of space separated numbers, calculate the product of all the sums of the digits of each number.
-- e.g. given "3 32 12 50", the answer would be (3)*(3+2)*(1+2)*(5+0)=3*5*3*5=225, so you would output 225

-- p "3 32 12 50"
-- 225

main20=interact$p20
p20 s=show$f20<$>(tail$lines s)
f20 s=d20<$>read<$>words s::[Int]
d20 [a,b]=min (y-x) (abs $ y-100+x)
  where
   x=min a b
   y=max a b

main20_2=interact$unlines.map (show.(\[a,b]->min (mod (a-b) 100) (mod (b-a) 100)).map read.words).tail.lines

-- p "3\n3 7\n0 99\n30 40"

-- 4
-- 1
-- 10


main21=interact$p21
p21 s=show$drop=<<subtract 2.length$s
d21="JFMAMJJASONDJF"
f21=drop=<<subtract 2.length

main21_2=interact$f21_2$cycle "JFMAMJJASOND"
f21_2 (y:ys) (x:xs)
 | x == y=f21_2 ys xs
 | True     =f21_2 ys (x:xs)
f21_2 (y:_) []=pure y

main21_3=interact$pure.head.foldl(\x y->tail$dropWhile(/=y)x)(cycle"JFMAMJJASOND")

-- JFMAMJJASON
-- D

-- main22=interact$p22
-- p22 s=show$c22$read<$>lines s
-- c22 [t,r]=(\i->(/10)$round$i*10)<$>[r * cos a, r * sin a]
--  where a=pi*t/180
-- You have to convert Polar coordinates (θ, r) to normal (x, y) coordinates.
-- One line: x, y separated by ", " (comma and space)
-- The x and y coordinates will have to be rounded of to one decimal place even if there are no decimals.

-- p "30\n12"
-- 10.4, 6.0

main22_2=interact$intercalate", ".map(printf"%0.1f").(\[a,r]->let a'=(a::Double)*pi/180in[r*cos a',r*sin a']).map read.words

main23=interact$unlines.(unwords.reverse<$>).transpose.map words.drop 2.lines

-- Take an NxM grid of numbers and return the MxN grid that results from rotating it 90 degrees clockwise.

-- p "2\n3\n1 2 3\n4 5 6"
-- 4 1
-- 5 2
-- 6 3

main24=interact$p24
p24 s=show$product$map signum$z$(\c->read c::Int)<$>words s
 where z [a,b]=enumFromTo a b

-- p "-9 -6"
-- 1

main25=interact$show.product.map signum.z25.(read<$>).words where z25[a,b]=enumFromTo a b

main26=interact$unlines.group.sort.map toLower.filter isAlpha.last.lines

-- p "10\nabcabcabcb"

-- aaa
-- bbbb
-- ccc

main27=interact$p27
p27 s=show$zip a (concat$repeat b)
 where(a,b)=(!!2)&&&(!!3)$lines s

-- p "9\n5\n2 29 18 19 17 6 15 27 15\nAPPLE"

-- CODINGAME

main28=interact$p28
p28 s=show$y
 where
  x=words s
  y=filter((=='c').head)x
  z=if null y then s++"covfefe"else""

u=unwords
g28_2 w i|i==w="covfefe"|1<2=i
k28_2 i|elem"covfefe"i=u i|1<2=(u i)++"covfefe"
h28_2 l=last$[" "]++[w|w<-l,(length w)==maximum(map length l)]
f28_2 i=k28_2$map(g28_2$h28_2(filter(\x->head x=='c')i))i
main28_2=interact$f28_2.words

-- Your task is to replace the longest word starting by c in a tweet by covfefe.
-- If no words starts by c, put covfefe at the end. If multiple words start by c and have the same length, choose the last to appear in the sentence.

-- p "I have good coverage of spray tan"

-- I have good covfefe of spray tan

-- main29=interact$p29
-- p29 s=show$unlines$map (\s->sort$filter (\c ->c `elem` "aeiouAEIOU") s)$tail$lines s

-- import Data.Maybe
-- import Data.List
-- import qualified Data.List.NonEmpty as N
-- import Data.Char
-- main=interact$unlines.map(fromMaybe"NONE".fmap(pure.N.head).N.nonEmpty.sortOn toLower.filter(`elem`"aeiouAEIOU")).tail.lines

-- p "3\nPLOP\nXPLDR\nLOLILOL"

-- O
-- NONE
-- I

main30=interact$p30
p30 s=head$last$sortOn length$group$sort$subsequences s

main31=interact$p31
p31 s=y
 where
  [a,b]=lines s
  x=take(length b)$concat$repeat a
  y=map(\c->b!!read [c])x

-- p "231450\n state below"
-- taste elbow

-- import qualified Data.Text as T
-- main=interact$p
-- p s=show$T.zipWith (T.append) x y
--  where
--   (x:y)=T.splitOn " " s

-- import Data.List.Split
-- g w(l,i)=[w!!i]++l
-- f(i:j)=unwords$map(g i)$zip j[0..]
-- main=interact$f.(splitOn" ")

-- p "Tiam his s  essage"
-- This is a message


-- import Data.List
-- w=words
-- g n(i,j)=div(j*100)n==i
-- f[n,l]=[i|(i,j)<-filter(g(read n))[(read v,length$filter(==v)$w l)|v<-(nub(w l))]]
-- main=interact$show.sum.f.lines