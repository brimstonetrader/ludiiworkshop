
def sort(ps): #Manhattan Distance from (0,0)
    if ps == []: #Wrong kind of not deterministic oops
        return ps
    elif len(ps) == 1:
        return ps
    elif len(ps) == 2:
        if sum(ps[0]) > sum(ps[1]):
            return ps
        else:
            return [ps[1],ps[0]]
    m = len(ps) // 2
    return collate(sort(ps[:m]),sort(ps[m:])) #There's gotta be a better way than this in python to split a list.
                                          
def collate(xs, ys):
    if len(ys) == 0:
        return xs
    if len(xs) == 0:
        return ys
    x,y = sum(xs[0]), sum(ys[0])
    if x > y: 
        zs = collate(xs[1:],ys)
        zs.append(xs[0])
        return zs
    else:
        zs = collate(xs,ys[1:])
        zs.append(ys[0])
        return zs