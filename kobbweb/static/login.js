newMember = false;

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

function submit(obj) {
    $('#status').hide();

    var method = 'PUT';

    if (obj.passwordVerify.length !== 0)
        method = 'POST'

    var submitData = JSON.stringify(obj);
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

function submitOnEnter(e, submitDelegate) {
    if (e.charCode == 13) {
        submitDelegate();
    }
}

function loginDelegate() { 
    submit({
        email : $('#email').val(),
        password : $('#password').val(),
        passwordVerify : ''
    });
}

function joinDelegate() {
    submit({
        email : $('#join-email').val(),
        password : $('#join-password').val(),
        passwordVerify : $('#join-password-verify').val()
    });
}

function inviteDelegate() {
    var email = $('#invitation').val();
    if (email.indexOf('@') !== -1) {
        var submitData = JSON.stringify({ email : email });
        var ajaxRequest = {
            url : '/beta',
            type : 'POST',
            dataType : 'json',
            data : submitData,
            processData : false,
            success : function() {
                $('#invite-box').html("<hr/><h3>Thank you for your interest!</h3><p>We'll be in touch!</p>");
            }
        };
        $.ajax(ajaxRequest)
    }
}

function init() {
    $('#submit').click(loginDelegate);
    $('#join').click(joinDelegate);
    $('#login-form').keypress(function(e) { 
            submitOnEnter(e, loginDelegate); 
        });
    $('#join-form').keypress(function(e) { 
            submitOnEnter(e, joinDelegate); 
        });
    $('#invite-submit').click(inviteDelegate);
}
