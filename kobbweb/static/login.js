newMember = false;

function showJoinForm() {
    $('#password-verify-input').show();
    $('#new').hide();
    $('#submit').text('Join!');
    newMember = true;
}

function sessionCreated() {
    window.location = "/home";
}

function setLoginStatus(statusCode) {
    $('#status').text(statusCode).show().addClass("error");
}

function sessionCreationError(xhr, textStatus, errorThrown) {
    if (xhr.status >= 400 && xhr.status < 500) {
        var response = JSON.parse(xhr.responseText);
        if (response.success === "false") {
            setLoginStatus(response.reason);
        }
    } else if (xhr.status == 500) {
        window.location = '/500';
    }
}

function submit() {
    $('#status').hide();

    var email = $('#email').val();
    var password = $('#password').val();
    var passwordVerify = $('#password-verify').val();
    var method = 'PUT';

    if (passwordVerify.length !== 0)
        method = 'POST'

    var submitData = JSON.stringify({
        email : email,
        password : password,
        passwordVerify : passwordVerify
    });

    var ajaxRequest = {
        url : '/sessions',
        type : method,
        dataType : 'json',
        data : submitData,
        processData : false,
        success : sessionCreated,
        error : sessionCreationError
    };
    $.ajax(ajaxRequest)
}

function submitOnEnter(e) {
    if (e.charCode == 13) {
        submit();
    }
}

function init() {
    $('#new').click(showJoinForm);
    $('#submit').click(submit);
    $('#login-form').keypress(submitOnEnter);
}
