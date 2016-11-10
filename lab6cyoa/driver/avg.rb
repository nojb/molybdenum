        def geometricAverage(autoscores,refscores)
                  product = 1.0
                  num = 0
                  refscores.each { |r|
                     acycles = autoscores.shift.to_f
                     rcycles = r.to_f
                     if acycles > 0.0 then
                       num = num + 1
                       product = product * (acycles/rcycles)
                     else
                       return 0.0
                     end
                  }
                  score = product ** (1.0 / num)
                  return score
        end

def ga (autoscores,refscores)
  sum = 0.0
  num = 0
  refscores.each { |r|
    acycles = autoscores.shift.to_f
    rcycles = r.to_f
    num = num + 1
    sum = sum + Math.log(acycles/rcycles)
  }
  sum = sum / num
  return Math.exp(sum)
end

def ex1 ()
  return "236631:131636:186814:84299:165377:184936:186770:123602:153958:100322:258790:55833:127415:4762668:1479381:4019805".split(":")
end
def refO0scores()
  return "818317:280913:432624:342203:246338:353231:523877:283678:172261:108240:444379:113737:193259:14148909:3675259:12578161".split(":")
end


print "%9.7f" % geometricAverage(ex1, refO0scores) + "\n"
print "%9.7f" % ga(ex1, refO0scores) + "\n"

