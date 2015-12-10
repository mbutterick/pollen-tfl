<!DOCTYPE html>
<html>
<head>
    <script type="text/javascript">

    function get_query_variable(key)
    {
           var query = window.location.search.substring(1);
           var vars = query.split("&");
           for (var i=0;i<vars.length;i++) {
                   var pair = vars[i].split("=");
                   if(pair[0] == key){return pair[1];}
           }
           return(false);
    }

    function finish_form_with_query_variable(key)
    {
        var query_value = get_query_variable(key);
        if (query_value != false) {
            document.getElementById('post_form_item').value = query_value;
            document.forms['post_form'].submit();
        }
    }

    </script>
</head>
<body onLoad='finish_form_with_query_variable("item")'>
    <form id="post_form" method="post" action="http://buy.mbtype.com/cart">
        <input id="post_form_item" name="cart[add][id]" type="hidden" />
    </form>
</body>
</html>