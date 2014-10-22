(function($) {
    var RECAPTCHA_PUBLIC_KEY = "6Lc4QfwSAAAAANMllvNwsOJ4hsX3ABIAXgXzjqqG";
    var RE_MAIL = /.+@[^@]+\.[^@]{2,}$/;
    var RE_ANY = /^.+$/;
    var opts = ['login', 'reg', 'reset', 'update', 'logout'];

    $.app = {}
    $.app.init = function() {
        api('user/info', {}, function(m) {
            switch(m.result) {
                case 'ok': {
                    $("#id_info_mail").text(m.mail);
                    $("#id_info_name").text(m.name);
                    $("ul.nav a[href='#info']").removeClass("hide").tab('show');
                    $("ul.nav a[href='#update']").removeClass("hide");
                    break;
                }
                default: {
                    var h = window.location.hash.substring(1);                    
                    var a = (h && h.length > 1) ? h.split(':', 2) : ['login','']; 
                    switch(a[0]) {
                        case 'update': {
                            if(a[1] && a[1].match(/[a-zA-Z0-9]+/)) {
                                $("#id_update_token").val(a[1]);
                                $("ul.nav a[href='#update']").removeClass("hide").tab('show');
                            } else {
                                go('/');
                            }
                            break;
                        }
                        default: {
                            $("ul.nav a[href='#login']").removeClass("hide");
                            $("ul.nav a[href='#reg']").removeClass("hide").click(function() {
                                Recaptcha.create(RECAPTCHA_PUBLIC_KEY, "reg_recaptcha", { theme: "clean" });
                            });
                            $("ul.nav a[href='#reset']").removeClass("hide").click(function() {
                                Recaptcha.create(RECAPTCHA_PUBLIC_KEY, "reset_recaptcha", { theme: "clean" });                  
                            });
                            $("ul.nav a[href='#" + (a[0].match(/login|reg|reset/) ? a[0] : 'login') + "']").click();
                        }
                    } 
                    break;
                }
            }
            $("ul.nav a").click(function() { reset(); });
        });

        for(i = 0; i < opts.length; i++) {
            $.app[opts[i]] = each(i);
        }
    }

    function each(i) {
        var opt = opts[i];

        return function() {
            if(check(opt)) api(opt, $("#" + opt + "_form").serialize(), function(d) { error(d, opt) } );
        }
    }

    function check(me) {
        reset();

        r = true;
        switch(me) {
            case 'login': {
                r = item(me, 'mail', RE_MAIL) && item(me, 'password', RE_ANY);
                break;
            }
            case 'reg': {
                r = item(me, 'mail', RE_MAIL) && item(me, 'name', RE_ANY);
                break;
            }
            case 'reset': {
                r = item(me, 'mail', RE_MAIL);
                break;
            }
            case 'update': {
                r = item(me, 'password', RE_ANY) && item(me, 'password2', RE_ANY) && comp(me, 'password', 'password2');
                break;
            }
        }

        if(r) {
            alerts("loading");
            $("#" + me + "_button").attr("disabled", "disabled");
        };
        return r;
    }

    function item(f, el, re) {
        var i = $("#id_" + f + "_" + el);
        var r = i.val().match(re);
        
        if(!r) invalid(i);
        return r;
    }

    function comp(f, el1, el2) {
        var i1 = $("#id_" + f + "_" + el1);
        var i2 = $("#id_" + f + "_" + el2);
        var r = i1.val() == i2.val();

        if(!r) { invalid(i1); invalid(i2); }
        return r;
    }

    function invalid(i) {
        i.parent().parent().addClass("has-error");
        i.focus(function() { i.parent().parent().removeClass("has-error"); });    
    }

    function error(me, opt) {
        no("loading");
        if(me.result == 'error') {
            alerts(me.error);
            $("#" + opt + "_button").removeAttr("disabled");        
        } else {
            switch(opt) {
                case 'login': { go('/#info'); break; }
                case 'reg': { alerts('reg_success'); break; }
                case 'reset': { alerts('reset_success'); break; }
                case 'update': { alerts('update_success'); setTimeout(function() { go('/#info'); }, 5000); break;}
                case 'logout': { go('/'); break; }
            }
        }
    }

    function alerts(i) {
        $("#a_" + i).removeClass("hide");
    }

    function no(i) {
        $("#a_" + i).addClass("hide");
    }

    function reset() {
        $("button").removeAttr("disabled");
        $("div.alert").addClass("hide");
    }
    
    function go(l) {
        window.location.href = l;
    }

    function api(func, params, callback) {
        $.post('/auth/' + func, params, function(d) { callback(d ? d : { result: 'error', error: 'no_server' }); });
    }

})(window.jQuery);

$(document).ready(function() { $.app.init(); });