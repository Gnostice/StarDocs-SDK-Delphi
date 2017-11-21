/* 
 * Gnostice StarDocs
 * Copyright © 2002-2017 Gnostice Information Technologies Private Limited, Bangalore, India
 * http://www.gnostice.com
 * 
*/

$(document).ready(function() {
	// 'Ext.panel.iframe' is UniGUI's custom component
	// We are defining our component after all the JS files are loaded since otherwise 'Ext.panel.iframe' may not be defined
	// in which case Ext assumes that 'Ext.panel.iframe' is it's component and tries to load a JS file which won't exist
	Ext.define('Ext.gnostice.stardocsviewer', {
		extend: 'Ext.panel.iframe',
		alias: 'widget.stardocsviewer',
		gnosticeViewerIf: null,
		
		constructor: function(cfg) {
			this.callParent(arguments);
			
			// Add event handler
			this.on('frameload', this.afterLoad);
		},

		initComponent: function() {
				var me = this;
				me.callParent(arguments);
				
				// Add events
        me.addEvents(
					'onFormLoaded',
					'onFormModified',
					'onBeforeFormSubmit',
					'onAfterFormSubmit',
					'onFieldClicked',
					'onFieldFocusIn',
					'onFieldFocusOut',
					'onFieldChanged'
				);
		},
		
		// Handle frameload event to conntect to viewer in the frame
		afterLoad: function() {
			var me = this;
			// Ignore first-time form load event (UniGUI sets the URL to 'about:blank' which gets loaded)
			if (me.iframe.src == null || me.iframe.src.indexOf('/stardocs/') == -1) {
				return;
			}
			//console.log('In afterLoad');
			// Establish a channel to the viewer
			var frameId = me.iframe.id;
			me.gnosticeViewerIf = new Gnostice.StarDocsViewerInterface(frameId, me.iframe.src);
			// Subscribe to events
			me.gnosticeViewerIf.forms.addEventListener('onFormLoaded', null, function(args) {
				//console.log('gnosticeViewerIf: onFormLoaded');
				me.fireEvent('onFormLoaded', me, args);
			});
			me.gnosticeViewerIf.forms.addEventListener('onFormModified', null, function(args) {
				//console.log('gnosticeViewerIf: onFormModified');
				me.fireEvent('onFormModified', me, args);
			});
			me.gnosticeViewerIf.forms.addEventListener('beforeFormSubmit', null, function(args) {
				//console.log('gnosticeViewerIf: onBeforeFormSubmit');
				var strFormData = null;
				if (args['keyValuePair'] != null) {
					strFormData = JSON.stringify({keyValuePair: args['keyValuePair']});
				}
				me.fireEvent('onBeforeFormSubmit', me, strFormData);
			});
			me.gnosticeViewerIf.forms.addEventListener('afterFormSubmit', null, function(args) {
				//console.log('gnosticeViewerIf: onAfterFormSubmit');
				me.fireEvent('onAfterFormSubmit', me, args);
			});
			me.gnosticeViewerIf.forms.addEventListener('onFieldClicked', null, function(args) {
				//console.log('gnosticeViewerIf: onFieldClicked');
				var strFormField = null;
				if (args['formField'] != null) {
					strFormField = JSON.stringify(args['formField']);
				}
				me.fireEvent('onFieldClicked', me, strFormField);
			});
			me.gnosticeViewerIf.forms.addEventListener('onFieldFocusIn', null, function(args) {
				//console.log('gnosticeViewerIf: onFieldFocusIn');
				var strFormField = null;
				if (args['formField'] != null) {
					strFormField = JSON.stringify(args['formField']);
				}
				me.fireEvent('onFieldFocusIn', me, strFormField);
			});
			me.gnosticeViewerIf.forms.addEventListener('onFieldFocusOut', null, function(args) {
				//console.log('gnosticeViewerIf: onFieldFocusOut');
				var strFormField = null;
				if (args['formField'] != null) {
					strFormField = JSON.stringify(args['formField']);
				}
				me.fireEvent('onFieldFocusOut', me, strFormField);
			});
			me.gnosticeViewerIf.forms.addEventListener('onFieldChanged', null, function(args) {
				//console.log('gnosticeViewerIf: onFieldChanged');
				var strFormField = null;
				if (args['formField'] != null) {
					strFormField = JSON.stringify(args['formField']);
				}
				me.fireEvent('onFieldChanged', me, strFormField);
			});
		},
		
		getAllFormFields: function() {
			//console.log('In getAllFormFields');
			var me = this;
			me.gnosticeViewerIf.forms.getAllFormFields(function(formFields) {
				var strFormFields = JSON.stringify(formFields);
				ajaxRequest(me, 'getAllFormFields', ['gnParam0=' + strFormFields]);
			});
		},
		
		getFormField: function(formFieldName) {
			//console.log('In getFormField');
			var me = this;
			me.gnosticeViewerIf.forms.getFormField(formFieldName, function(formField) {
				var strFormField = JSON.stringify(formField);
				ajaxRequest(me, 'getFormField', ['gnParam0=' + strFormField]);
			});
		},

		submitForm: function(submitUrl, submitMethod, includeNoValueFields, submitFieldsCSV, isIncludeList) {
			//console.log('In submitForm');
			var submitFields = null;
			if (submitFieldsCSV != null && submitFieldsCSV.length > 0) {
				submitFields = submitFieldsCSV.split(',');
			}
			var me = this;
			me.gnosticeViewerIf.forms.submitForm(submitUrl, submitMethod, includeNoValueFields, submitFields, isIncludeList, 
				function(data, textStatus) {
					// Success CB
					ajaxRequest(me, 'submitFormSuccess', ['gnParam0=' + data, 'gnParam1=' + textStatus]);
				}, function(textStatus, errorThrown) {
					// Failure CB
					ajaxRequest(me, 'submitFormFailure', ['gnParam0=' + textStatus, 'gnParam1=' + errorThrown]);
				});
		},

		resetForm: function(resetFieldsCSV, isIncludeList) {
			//console.log('In resetForm');
			var resetFields = null;
			if (resetFieldsCSV != null && resetFieldsCSV.length > 0) {
				resetFields = resetFieldsCSV.split(',');
			}
			this.gnosticeViewerIf.forms.resetForm(resetFields, isIncludeList);
		},

		focusForm: function() {
			//console.log('In focusForm');
			this.gnosticeViewerIf.forms.focusForm();
		},
		
		setFocus: function(formFieldName) {
			//console.log('In setFocus');
			this.gnosticeViewerIf.forms.setFocus(formFieldName);
		},

		getValueAsString: function(formFieldName) {
			//console.log('In getValueAsString');
			var me = this;
			me.gnosticeViewerIf.forms.getValueAsString(formFieldName, 
				function(value) {
					// Success CB
					ajaxRequest(me, 'getValueAsString', ['gnParam0=' + value]);
				});
		},

		setValueAsString: function(formFieldName, value) {
			//console.log('In setValueAsString');
			this.gnosticeViewerIf.forms.setValueAsString(formFieldName, value);
		},

		getRadioButtonSelectedIndex: function(formFieldName) {
			//console.log('In getRadioButtonSelectedIndex');
			var me = this;
			me.gnosticeViewerIf.forms.getRadioButtonSelectedIndex(formFieldName, 
				function(index) {
					// Success CB
					ajaxRequest(me, 'getRadioButtonSelectedIndex', ['gnParam0=' + index]);
				});
		},

		setRadioButtonSelectedIndex: function(formFieldName, index) {
			//console.log('In setRadioButtonSelectedIndex');
			this.gnosticeViewerIf.forms.setRadioButtonSelectedIndex(formFieldName, index);
		},

		getComboListBoxSelectedItemIndices: function(formFieldName) {
			//console.log('In getComboListBoxSelectedItemIndices');
			var me = this;
			me.gnosticeViewerIf.forms.getComboListBoxSelectedItemIndices(formFieldName, 
				function(indices) {
					// Success CB
					// Convert array of ints to CSV
					var csv = indices.join();
					ajaxRequest(me, 'getComboListBoxSelectedItemIndices', ['gnParam0=' + csv]);
				});
		},

		setComboListBoxSelectedItemIndices: function(formFieldName, indices) {
			//console.log('In setComboListBoxSelectedItemIndices');
			this.gnosticeViewerIf.forms.setComboListBoxSelectedItemIndices(formFieldName, indices);
		},

		getCheckBoxChecked: function(formFieldName) {
			//console.log('In getCheckBoxChecked');
			var me = this;
			me.gnosticeViewerIf.forms.getCheckBoxChecked(formFieldName, 
				function(checked) {
					// Success CB
					ajaxRequest(me, 'getCheckBoxChecked', ['gnParam0=' + checked]);
				});
		},

		setCheckBoxChecked: function(formFieldName, checked) {
			//console.log('In setCheckBoxChecked');
			this.gnosticeViewerIf.forms.setCheckBoxChecked(formFieldName, checked);
		}

	});
});
