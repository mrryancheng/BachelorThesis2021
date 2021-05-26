$(".ticket-symbol").click(function()
{var publicKey=document.getElementById('publicKey').value;var ticketVal=$(this).text();var languagecode="";$(".ticket-symbol").attr('data-toggle','modal');if(location.pathname.indexOf('/fr/')>-1)
{$('#stock-modal-content h3').text("'L'information financière: "+ticketVal);languagecode="fr";}
else{$('#stock-modal-content h3').text('Financial Information: '+ticketVal);}
var divValue='<div class="modal-body" id="modal-body" style="height: 300px;"><div class="stockquote" id="stockquote"></div> <script type="text/javascript">new currentWidget({ el: document.getElementById("stockquote"), authHeader:"publicKey '+publicKey+'",symbol:"'+ticketVal+'",lang:"'+languagecode+'"}).render();<\/script></div>';if($('#stock-modal-content #modal-body').length==0){$('#stock-modal-content').append(divValue);}
else{$('#stock-modal-content #modal-body').replaceWith(divValue);}});