#include <astro/remoteinstance.h>

#include <iostream>
#include <unistd.h>
#include <memory.h>

class Server : public ServerInstance {
public:
	virtual int run() {
		cout << "Server\n";

		const char *host = argv[1];
		char cmd[200];
		sprintf(cmd, "%s %s", argv[0], host);

//		const char *cmd = "/u/mjuric/science/satools/tests/parallel/a.out";
//		const char *host = "krampus.princeton.edu";

		RemoteInstance *krampus = spawn(cmd, host);
		cout << "Client spawned and connected!\n";

//		sleep(3);
		krampus->write("12345", 5);
		cout << "Written half...\n"; cout.flush();
//		sleep(3);
		krampus->write("12345", 5);

		cout << "Deleting krampus\n"; cout.flush();
		delete krampus;

//		sleep(2);
		cout << "Creating new krampus\n"; cout.flush();
		krampus = spawn(cmd, host);
		
//		sleep(3);
		krampus->write("9876543210", 10);

		cout << "Server exiting\n";
		return 0;
	}
};

class Client : public RemoteInstance {
public:
	virtual int run() {
		cout << "*** I'm a client!\n";
		cout.flush();

		cout << "*** Trying to read\n"; cout.flush();
		char buf[1000] = {0}; int n;

		while(isConnected()) {
			if(tryRead(buf, 10) && isConnected()) {
				cout << "*** Client got: [" << buf << "]\n"; cout.flush();
				memset(buf, 0, 1000);
			} else {
				cout << "*** Sleeping 1 second...\n"; cout.flush();
				sleep(1);
			}
		}

		return 0;
	}
};

MAIN(Server, Client)
