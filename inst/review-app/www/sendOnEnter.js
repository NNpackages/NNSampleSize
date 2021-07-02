// This script just listens for "enter"s on the username input and simulates
// clicking the "login" button when that occurs.
$(document).keyup(function(event) {
    if ($("#username").is(":focus") && (event.key == "Enter")) {
        $("#login").click();
    }
});
