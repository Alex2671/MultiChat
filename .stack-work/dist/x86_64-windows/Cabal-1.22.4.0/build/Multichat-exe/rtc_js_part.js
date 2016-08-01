document.addEventListener("DOMContentLoaded", function(event) {

    var nickname = prompt("Enter a name for the contact list");

    if (nickname === null || nickname === "") {
        alert ("You must enter a name, to be identified on the server");
        return;
    }

    // Will hold our peer's nickname
    var peer = null;

    // Here we are using Google's public STUN server, and no TURN server
    var ice = { "iceServers": [{"url": "stun:stun.l.google.com:19302"}]};
    var pc = null; // This variable will hold the RTCPeerConnection

    document.getElementById('nickname').innerHTML = nickname;

    var constraints = {
        video: "true",
        audio: "true" };

    // Prevent us to receive another call or make another call while already in one
    var isInCall = false;

    // Specify if we have to create offers or answers
    var isCaller = false;

    var receivedOffer = null;

    // For portability's sake
    navigator.getUserMedia = navigator.getUserMedia || navigator.webkitGetUserMedia || navigator.mozGetUserMedia;
    window.RTCPeerConnection = window.RTCPeerConnection || window.mozRTCPeerConnection || window.webkitRTCPeerConnection;
    window.RTCSessionDescription = window.RTCSessionDescription || window.mozRTCSessionDescription || window.webkitRTCSessionDescription;
    window.RTCIceCandidate = window.RTCIceCandidate || window.mozRTCIceCandidate || window.webkitRTCIceCandidate;

    // Open a connection to our server
    var socket = new WebSocket('ws://192.168.1.3:4001');

    // Display an error message if the socket fails to open
    socket.onerror = function(err) {
        alert("Failed to open connection with WebSockets server.\nEither the server is down or your connection is out.");
        return;};

    // Provide visual feedback to the user when he is disconnected
    socket.onclose = function (evt) {
        document.getElementById("status").innerHTML = "<strong class=\"red\">disconnected!</strong>";
        };

    // When the connection is opened, the server expects us to send our nickname right away
    socket.onopen = function() {
        document.getElementById("status").innerHTML = "<strong class=\"green\">connected!</strong>";
        socket.send (JSON.stringify ({"nickname": nickname}));
     };

    // Process incomming messages from the server, can be the user list or messages from another peer
    socket.onmessage = function (msg) {
        // Parse message, JSON is used for all message transfer
        try {
            var dat = JSON.parse (msg.data);
        } catch(e) {
            console.log ("ERROR - Received wrong-formatted message from server:\n" + e);
            socket.close();
            isInCall = false;
            isCaller = false;
            return;
        }

        // Process userlist : display the names in the contact list
        if (dat.userlist) {
            var l = dat.userlist;
            var domContent = "";

            // Add each user on the list and register a callback function to initiate the call
            l.forEach (function (elem) {
                // Filter out our name from the list: we don't want to call ourselve!
                if (elem !== nickname) {
                    domContent += "<li><button onclick='navigator.callUser(\"" + elem + "\");'>" + elem + "</button></li>";
                }
             });

            // Add the generated user list to the DOM
            document.getElementById("userlist").innerHTML = domContent;
         }
        // If the message is from a peer
        else if (dat.from) {
            // When we receive the first message form a peer, we consider ourselved in a call
            if (!isInCall) {
                isInCall = true;
                peer = dat.from;
                document.getElementById('calling_status').innerHTML = peer + " is calling...";
            }

            if (dat.offer) {
                receivedOffer = dat.offer;
                startConv();
            } else if (dat.answer) {
                pc.setRemoteDescription (new RTCSessionDescription (dat.answer),
                                         function () {
                                            console.log ("Set remote description - handshake complete.");
                                            // As we are now in a call, log out from the signalling server
                                            socket.close();
                                         },
                                         function () {
                                            console.log ("Failed to set remote description - handshake failed.");
                                         });
            }
        }

        // Otherwise, this is an error
        else {
            alert ("Received a non-intended message.");
            socket.close();
            isInCall = false;
            isCaller = false;
            return;
        }
     }// end of 'socket.onmessage()'

    // Initiate a call to a user
    navigator.callUser = function (who) {
        document.getElementById('calling_status').innerHTML = "Calling " + who + " ...";
        isCaller = true;
        peer = who;
        startConv();
     }

    // Start a call (caller) or accept a call (callee)
    function startConv() {
        if (isCaller) {
            console.log ("Initiating call...");
        } else {
            console.log ("Answering call...");
        }

        // First thing to do is acquire media stream
        navigator.getUserMedia (constraints, onMediaSuccess, onMediaError);
     } // end of 'startConv()'

    function onMediaError (err) {
        alert ("Media was denied access: " + err);
        document.getElementById('calling_status').innerHTML = "";
        socket.close();
        isCaller = false;
        isInCall = false;
        return;
     }

    function onMediaSuccess (mediaStream) {
        // Hide the contact list and show the screens
        document.getElementById("signalling").style.display = "none";
        document.getElementById("oncall").style.display = "block";

        // Display our video on our screen
        document.getElementById("localVideo").src = URL.createObjectURL(mediaStream);

        // Create the RTCPeerConnection and add the stream to it
        pc = new window.RTCPeerConnection (ice);
        
        // Stream must be added to the RTCPeerConnection **before** creating the offer
        pc.addStream (mediaStream);

        pc.onaddstream = onStreamAdded;
        pc.onicecandidate = onIceCandidate;

        if (isCaller) {
            // Calling 'createOffer()' will trigger ICE Gathering process
            pc.createOffer (function (offerSDP) {
                pc.setLocalDescription (new RTCSessionDescription (offerSDP),
                                        function () {
                                            console.log ("Set local description");
                                        },
                                        function () {
                                            console.log ("Failed to set up local description");
                                        });
            },
                            function (err) {
                console.log ("Could not build the offer");
            }, constraints);

        } else {
            pc.setRemoteDescription (new RTCSessionDescription (receivedOffer),
                                     function () {
                                        pc.createAnswer (function (answerSDP) {
                                            pc.setLocalDescription (new RTCSessionDescription (answerSDP),
                                                                    function () {
                                                                        console.log ("Set local description");
                                                                    },
                                                                    function () {
                                                                        console.log ("Failed to set up local description");
                                                                    });
                                        },
                                                        function (err) {
                                            console.log ("Could not build the answer");
                                        }, constraints);

                                     },
                                     function () {
                                        console.log ("Failed to set up remote description");
                                    });
        }
     }  // end of 'onMediaSuccess()'

    function onStreamAdded (evt) {
        console.log ("Remote stream received");
        document.getElementById("remoteVideo").src = URL.createObjectURL(evt.stream);
     }// end of 'onStreamAdded()'

    function onIceCandidate (evt) {
        // Wait for all candidates to be gathered, and send our offer to our peer
        if (evt.target.iceGatheringState === "complete") {
            console.log ("ICE Gathering complete, sending SDP to peer.");

            // Haven't found a way to use one-line condition to substitute "offer" and "answer"
            if (isCaller) {
                var offerToSend = JSON.stringify ({ "from": nickname,
                                                    "offer": pc.localDescription
                                                });
                socket.send( JSON.stringify( {"target": peer, "sdp": offerToSend}));
                console.log ("Sent our offer");
            } else {
                var answerToSend = JSON.stringify ({ "from": nickname,
                                                    "answer": pc.localDescription
                                                });
                socket.send( JSON.stringify( {"target": peer, "sdp": answerToSend}));
                console.log ("Sent our answer");
                // Once we sent our answer, our part is finished and we can log out from the signalling server
                socket.close();
            }
        }
     }
 });