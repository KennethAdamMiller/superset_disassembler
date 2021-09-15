from malamute import MalamuteClient

#TODO setup the cli arguments: addr, corpora location (if any)

service = MalamuteClient()
service.connect(addr, 100, b'service')
service.set_worker(b'service', b'derps')

#TODO collect all of the binaries from args and send them one by one?

#how are results collected?
    #results can be gathered and added to the local cache, cleared from the origin
    #or the values required can be requested using the hash of each 
