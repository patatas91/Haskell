--import ArrayPolynomial
import TupleListPolynomial

main = do
		print p
		print (peval p  2)
		print dp
		print (peval dp 2)
		where
			p  = padd [(pmul [x,x]),(coef 3),(pmul [(coef 2),x])]
			dp = pderv p

