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