webpackJsonp([6],{1795:function(i,e,t){"use strict";function s(i,e){var t=document.getElementById(i),s=void 0;return t.currentStyle?s=t.currentStyle[e]:window.getComputedStyle&&(s=document.defaultView.getComputedStyle(t,null).getPropertyValue(e)),s}Object.defineProperty(e,"__esModule",{value:!0});var a=t(9);t(1007);window.CSBar=window.Class.create(),window.CSBar.suspendRounding=!1,window.CSBar.prototype={initialize:function(i,e,t,s,r,n){this.type=n||"bar",this.QID,this.minValue=0,this.barMaxWidth=500,this.maxValue=100,this.totalMax=0,this.sliders=new Object,this.gridLines=5,this.snapToGrid=!1,this.labelWidth=void 0,this.gridLinePosition=0,this.decimals=0,this.rtl=!1,this.realQuestionId=s,-1!=this.realQuestionId.indexOf("_")&&(this.realQuestionId=this.realQuestionId.substring(this.realQuestionId.indexOf("_")+1,this.realQuestionId.length));var d=window.$(this.realQuestionId);if(d&&"QuestionOuter"!=d.className&&(d=window.$$(".QuestionOuter#"+this.realQuestionId))&&(d=d[0]),!window.$(d)||!window.$(d).offsetWidth)return this.loaded=!1,!1;"rtl"===window.$(document.body).getStyle("direction")&&(this.rtl=!0),void 0!==e&&(this.maxValue=Number(e)),void 0!==i&&(this.minValue=Number(i)),void 0!==t&&(this.gridLines=t),s&&(this.QID=s),window.$(this.QID+"~totalMax")&&(this.totalMax=window.$(this.QID+"~totalMax").value,this.totalMax=parseInt(this.totalMax)),void 0!==r&&(this.snapToGrid="1"==r),this.animation=!1,this.resizeObserver=new a.e.observe(window,"resize",this.reCalculateMaxWidth.bind(this)),this.onLoadObserver=new a.e.observe(window,"load",this.reCalculateMaxWidth.bind(this)),this.loaded=!0},calculateTableMinWidth:function(){var i=window.$("LabelDescriptions~"+this.realQuestionId),e=0,t=0,s=0,a=0;i&&(e=i.offsetWidth,"100%"!=i.style.width&&(e*=100/parseFloat(i.style.width)),t=i.parentNode.offsetLeft),e<100&&(e=100);var r=window.$("ValueHeader~"+this.realQuestionId);r&&(s=r.offsetWidth);var n=window.$("NotApplicableHeader~"+this.realQuestionId);return n&&(a=n.offsetWidth)<100&&(a=100),e+t+s+a},reCalculateMaxWidth:function(){for(var i in this.sliders){var e=window.$(i+"~barTd");if(e){var t=e.parentNode,s=window.$(i+"~RightBorder");!1===this.rtl?this.barMaxWidth=s.offsetLeft-t.offsetLeft:this.barMaxWidth=window.$(i+"~LeftBorder").offsetLeft-s.offsetLeft,window.$(this.sliders[i].track).setStyle({width:this.barMaxWidth+"px"}),this.sliders[i].trackLength=this.barMaxWidth,this.sliders[i].value&&(this.suspend=!0,this.sliders[i].setValue(this.sliders[i].value),this.suspend=!1)}}},makeSlider:function(i,e){var t=window.$(i+"~barTd");if(t)try{var s=t.parentNode,a=window.$(i+"~RightBorder");this.barMaxWidth=Math.abs(a.offsetLeft-s.offsetLeft);var r=window.QBuilder("div",{id:i+"~track",className:"track"},[window.QBuilder("div",{id:i+"~handle",className:"handle"})]);window.$(r).setStyle({width:this.barMaxWidth+"px"});var n=window.QBuilder("div",{id:i+"~holder",className:"trackHolderRel"},[r,window.QBuilder("div",{id:i+"~bar",className:"bar"})]);t&&t.appendChild(n);var d=(this.barMaxWidth,this),l={onSlide:d.onSlide.bind(d,i),onChange:d.onSliderChange.bind(d,i)};if(this.snapToGrid){for(var o=[],h=0;h<=this.gridLines;h++)o[h]=h/this.gridLines;l.values=o}if(this.sliders[i]=new window.Control.Slider(i+"~handle",i+"~track",l),this.rtl&&(this.sliders[i].value=this.maxValue),this.sliders[i].choiceId=i.substring(i.indexOf("~")+1),this.snapToGrid){var u=window.$(i+"~ResultsTd");if(!u)return;if(!window.$(i+"~result~1"))for(var w=Number(this.gridLines)+1,v=1;v<=w;v++)u.appendChild(window.QBuilder("input",{type:"hidden",id:i+"~result~"+v,name:"QR~"+i+"~"+v}))}if(window.$(i+"~result")){var c=this.getResultValue(i);if(c){var f=this.valueToBar(c);this.sliders[i].setValue(f)}else void 0!=e&&(this.snapToGrid?this.sliders[i].setValue(Math.round(e*this.gridLines)/this.gridLines):this.sliders[i].setValue(e))}return window.$(i+"~result")&&(window.$(i+"~result").onchange=this.onValueEnter.bindAsEventListener(d,i)),window.$(i+"~NA")&&(window.$(i+"~NA").onclick=d.notApplicableHandler.bindAsEventListener(d,i),window.$(i+"~NA").checked&&(window.$(i+"~result~NA").value="1")),this.reCalculateMaxWidth.bind(this).defer(),this.sliders[i]}catch(i){console.log(i)}},onSlide:function(i,e,t){this.totalMax&&(e=this.enforceMaxValue(e,i));var s=e*this.barMaxWidth,a=window.$(i+"~bar");if(this.updateBar(a,s,!1),this.suspend)return!1;void 0!=window.$(i+"~result")?this.updateResultTextField(e,i):console.error("no input for "+i),this.sliders[i]&&!this.sliders[i].activated&&this.activateBar(i)},onSliderChange:function(i,e,t){if(this.onSlide(i,e,t),this.suspend)return!1;if(this.snapToGrid){this.suspend=!0;for(var s=(this.maxValue-this.minValue)/this.gridLines,a=Number(this.gridLines)+1,r=Math.round((this.barToValue(e)-this.minValue)/s+1),n=1;n<=a;n++)window.$(i+"~result~"+n).value="";window.$(i+"~result~"+r).value="Selected",this.suspend=!1}if(this.totalMax){this.getSliderMax(i)}this.onChange&&this.onChange(this.sliders[i])},getSliderMax:function(i){var e=this.totalMax,t=0,s=0,a=0;for(var r in this.sliders){var n=Number(window.$(r+"~result").value);isNaN(n)&&(n=0),s+=n,r!=i&&(t+=n)}return a=e-t,a-this.barToValue(this.sliders[i].value),window.$(this.QID+"~total").value=this.roundNumber(s,this.decimals),this.valueToBar(a)},updateBar:function(i,e,t){i&&(!0===this.rtl?i.style.width=this.barMaxWidth-e+"px":i.style.width=e+"px")},notApplicableHandler:function(i,e){if(a.e.element(i).checked)this.deactivateBar(e);else{var t=this.sliders[e],s=t.values.length>1?t.values:t.value;this.onSliderChange(e,s),this.onSlide(e,s)}},activateBar:function(i,e){this.sliders[i].activated=!0;var t=window.$(i+"~bar");window.$(t.parentNode).addClassName("activated"),window.$(i+"~NA")&&(window.$(i+"~NA").checked=!1,window.$(i+"~result~NA").value="",window.Qualtrics.syncLabelsAndInputs&&window.Qualtrics.syncLabelsAndInputs(!1)),e&&this.sliders[i].setValue(this.sliders[i].value)},deactivateBar:function(i){this.sliders[i].activated=!1;var e=window.$(i+"~bar"),t=window.$(i+"~result"),s=window.$(i+"~result~NA");e&&window.$(e.parentNode).removeClassName("activated"),t&&(t.value=""),s&&(s.value="1");for(var a=Number(this.gridLines)+1,r=1;r<=a;r++)window.$(i+"~result~"+r)&&(window.$(i+"~result~"+r).value="")},barToValue:function(i){var e=i*(this.maxValue-this.minValue)+this.minValue;return window.CSBar.suspendRounding?e:this.roundNumber(e,this.decimals)},valueToBar:function(i){var e=(i-this.minValue)/(this.maxValue-this.minValue);return e<0&&(e=0),e>1&&(e=1),!0===this.rtl&&(e=1-e),isNaN(e)?this.minValue:e},setDecimals:function(i){this.decimals=i},roundNumber:function(i,e){return Math.round(Math.round(i*Math.pow(10,e+1))/Math.pow(10,1))/Math.pow(10,e)},enforceMaxValue:function(i,e){var t=i,s=this.getSliderMax(e);return this.rtl?(t<s&&(t=s,this.sliders[e].setValue(s)),s>1&&(t=1,this.sliders[e].setValue(1))):(t>s&&(t=s,this.sliders[e].setValue(s)),s<0&&(t=0,this.sliders[e].setValue(0))),t},updateResultTextField:function(i,e){if(void 0!=window.$(e+"~result")){var t=void 0,s=this.getResultValue(e);if(t=!0===this.rtl?this.roundNumber(this.maxValue-this.barToValue(i)+this.minValue,this.decimals):this.barToValue(i),this.totalMax){this.getSliderMax(e)}if(!this.sliders[e].activated&&isNaN(s))return;window.$(e+"~result").hasClassName("IndividualResponseSummaryInput")?window.$(e+"~result").textContent=t:window.$(e+"~result").value=t,t!=s&&window.jQuery(window.$(e+"~result")).trigger("change")}},getResultValue:function(i){var e=window.$$("#"+i.replace("~","\\~")+"\\~result.IndividualResponseSummaryInput");return window.$(i+"~result").value||(e.length>0?e[0].textContent:"")},onValueEnter:function(i,e){if(this.suspend)return!1;var t=a.e.element(i);if(t){var s=t.value;if(""===s)this.deactivateBar(e);else{t.value=this.enforceLimits(s),this.activateBar(e),this.suspend=!0,s=this.roundNumber(s,this.decimals);var r=this.valueToBar(s);if(this.snapToGrid){var n=Math.round(r*this.gridLines)/this.gridLines;this.sliders[e].setValue(n);for(var d=(this.maxValue-this.minValue)/this.gridLines,l=Number(this.gridLines)+1,o=Math.round((this.barToValue(n)-this.minValue)/d+1),h=1;h<=l;h++)window.$(e+"~result~"+h).value="";window.$(e+"~result~"+o).value="Selected",window.$(e+"~result").value=this.roundNumber((o-1)*d+this.minValue,this.decimals)}else this.sliders[e].setValue(r);this.suspend=!1,this.totalMax&&(r=this.enforceMaxValue(r,e),this.updateResultTextField(r,e))}this.onChange&&this.onChange(this.sliders[e])}},enforceLimits:function(i){var e=this.minValue,t=this.maxValue;return e>t&&(e=this.maxValue,t=this.minValue),i<e&&(i=e),i>t&&(i=t),i},setStartPositionsArray:function(i){var e=window.$H(this.sliders).values();this.suspend=!0;for(var t=0,s=i.length;t<s;++t)e[t]&&!e[t].value&&e[t].setValue(i[t]);this.suspend=!1},setStartPositions:function(i){if(Object.isArray(i))return this.setStartPositionsArray(i);window.$H(this.sliders).values();this.suspend=!0;for(var e in i){var t=this.QID+"~"+e;"function"!=typeof i[e]&&(!this.sliders[t]||this.sliders[t].value||this.sliders[t].activated||(this.snapToGrid?this.sliders[t].setValue(Math.round(i[e]*this.gridLines)/this.gridLines):this.sliders[t].setValue(i[e])))}this.suspend=!1}},window.getStyle=s,e.default=window.CSBar}});