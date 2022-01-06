import sys
import os
sys.path.append(os.getcwd() + "/scripts")
from multiprocessing import Process
from client import worker
from distributor import dealer
import zmq
import unittest
from send_resources import send_host_resources

def run_dealer(d):
    d.run()

class Sender():
    def send(self, msg):
        pass
class Receiver():
    def recv(self):
        return b"test_hostname:example_file"
    
class TestDistributor(unittest.TestCase):
    def setUp(self):
        self.context=zmq.Context()
        self.d=dealer(1, ctxt=self.context)
        self.pdealer=Process(target=run_dealer, args=(self.d,))
        self.sndr=Sender()
        self.rcvr=Receiver()

    def tearDown(self):
        if self.pdealer.is_alive():
            self.pdealer.join()

    def sendResources(self, free_mem, free_cores):
        send_host_resources("tcp://localhost:9996", "test_hostname", free_mem, free_cores)

    def test_find_nearest(self):
        #test that the nearest file is found for the known resources
        self.d.add_to_size_map(3,"example_file")
        self.assertTrue(len(self.d.size_map)!=0)
        self.d.update_resources("test_hostname", 80000,24)
        proj=self.d.projection("test_hostname")
        self.assertTrue(proj is not None)
        fname,size=proj
        self.assertTrue(size==3)
        self.assertTrue(fname=="example_file", msg=("was: " + fname))
    
    def test_check_resources_fit(self):
        #test that files distributed fit within resources or are not distributed
        self.d.add_to_size_map(3,"example_file")
        self.assertTrue(len(self.d.size_map)!=0)
        self.d.update_resources("test_hostname",1000,24)
        proj=self.d.projection("test_hostname")
        self.assertTrue(proj is None)

    def test_no_exceed(self):
        #test that upon exceeding resources, the worker is put to sleep
        self.d.add_to_size_map(3,"example_file")
        self.assertTrue(len(self.d.size_map)!=0)
        self.d.update_resources("test_hostname",80000,0)
        proj=self.d.projection("test_hostname")
        self.assertTrue(proj is None)

    def test_size_conflict(self):
        self.d.add_to_size_map(5,"example_file")
        self.d.add_to_size_map(5,"example_file2")
        self.assertTrue(self.d.remaining()==2)
        self.d.update_resources("test_hostname",80000,24)
        proj=self.d.projection("test_hostname")
        self.assertTrue(self.d.remaining()==1)

    def test_resource_updates(self):
        self.d.add_to_size_map(3,"example_file")
        self.d.add_to_size_map(5,"example_file2")
        self.assertTrue(self.d.remaining()==2)
        self.d.update_resources("test_hostname",80000,24)
        proj=self.d.send_bin(self.sndr, "test_hostname")
        self.assertTrue(self.d.remaining()==1)
        self.assertTrue(proj is not None)
        self.d.update_resources("test_hostname",60000,23)
        self.assertTrue(self.d.resources.get("test_hostname")==[60000,23])
        proj=self.d.send_bin(self.sndr, "test_hostname")
        self.assertTrue(not self.d.above_outbound_limit("test_hostname"))
        self.assertTrue(self.d.outbound.get("test_hostname") is not None)
        self.assertTrue(self.d.remaining()==0)
        self.assertTrue(len(self.d.size_map.keys())==0)
        self.assertTrue(self.d.find_nearest(60000) is None)
        self.assertTrue(proj is not None)
        from collections import deque
        self.assertEqual(len(self.d.sent), 2)

    def test_nearest_below_resources(self):
        #TODO if a keys above current resources aren't picked and keys below are found
        pass
    
    def test_receive_results(self):
        #results of an analysis are put in results and removed from sent
        self.d.add_to_size_map(3,"example_file")
        self.d.update_resources("test_hostname",80000,24)
        proj=self.d.send_bin(self.sndr, "test_hostname")
        self.assertEqual(proj is not None, True)
        self.assertEqual(len(self.d.sent), 1)
        self.d.recv_results(self.rcvr)
        self.assertEqual(len(self.d.results), 1)
        self.assertEqual("example_file" in self.d.results, True)
        self.assertEqual("example_file" not in self.d.sent, True)

    def test_timeout_failure(self):
        self.d.worker_timeout=1
        #test that upon timeout, the work is re-added to the remaining set
        self.d.add_to_size_map(3,"example_file")
        self.d.update_resources("test_hostname",80000,24)
        proj=self.d.send_bin(self.sndr, "test_hostname")
        self.assertTrue(self.d.remaining()==0)
        self.assertTrue(proj is not None)
        fname,size,t=self.d.sent.pop()
        t=t-5 #adjust the time so that it looks like it has elapsed
        self.d.sent.appendleft([fname,size,t])
        self.d.check_sent_timeout()
        self.assertEqual(len(self.d.sent), 0)
        self.assertEqual(self.d.remaining() > 0, True)

if __name__ == '__main__':
    unittest.main()
