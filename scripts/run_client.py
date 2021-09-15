from malamute import MalamuteClient

writer = MalamuteClient()
print("writer.connect")
writer.connect(addr, 100, b'writer')
print("writer.set_producer")
writer.set_producer(b'writer')
