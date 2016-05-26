<h2>Some PHP page</h2>

<script>
	sendTestForm = function (obj) {
        var settings = {
            url: '/json.php',
            method: 'POST',
            body: 'username='+obj.elements['test-username'].value+'&password='+obj.elements['test-password'].value,
            returnType: 'json'
        };
        http.send(settings).done(function (data, status, headers) {
            if (data) {
                document.getElementById('test-username').value = data.username;
                document.getElementById('test-password').value = data.password;
            }
        })
        .fail(function (data, status, headers) {
            console.error(data);
        });
    };
</script>

<div style="margin: 10px;">
    <form action="" method="post" onsubmit="sendTestForm(this);return false;">
        <div>
            <label for="username">Username</label>
            <input type="text" id="username" class="" name="test-username">
        </div>
        <div>
            <label for="password">Password</label>
            <input type="password" id="password" class="" name="test-password">
        </div>

        <div>
            <button type="submit" class="" name="login-button">Send Data to WS</button>                
        </div>
    </form>
</div>
<div style="margin: 10px;">
    <h4>Your inputted data</h4>
    <div>
        <label>Username</label>
        <input type="text" id="test-username">
    </div>
    <div>
        <label>Password</label>
        <input type="text" id="test-password">
    </div>
</div>

<div style="margin: 10px;">
    <i>current year - <?= date('Y') ?></i>
</div>


