def player_strategy(n_battalions,n_fields):
    #defining the array:
    battalions=np.zeros(n_fields,dtype=int)
    
    #plassering av bataljoner 
    battalions[0:]=20
    battalions[1:]=10
    battalions[2:]=28
    battalions[3:]=10
    battalions[4:]=2
    battalions[5:]=30

    
    # Gjør de tilfeldig plassert
    battalions=battalions[np.random.rand(n_fields).argsort()]
    assert sum(battalions)==n_battalions
    
    return battalions