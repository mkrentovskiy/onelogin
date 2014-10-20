(function($) {
    var RECAPTCHA_PUBLIC_KEY = "6Lc4QfwSAAAAANMllvNwsOJ4hsX3ABIAXgXzjqqG";

    $.app = {}

    $.app.init = function() {
        Recaptcha.create(RECAPTCHA_PUBLIC_KEY, "reg_recaptcha", { theme: "clean" });
        Recaptcha.create(RECAPTCHA_PUBLIC_KEY, "reset_recaptcha", { theme: "clean" });
    }

})(window.jQuery);

$(document).ready(function() { $.app.init(); });