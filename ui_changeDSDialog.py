# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'Z:\dev\changeDataSource\ui_changeDSDialog.ui'
#
# Created: Tue Sep 29 13:40:52 2015
#      by: PyQt4 UI code generator 4.11.3
#
# WARNING! All changes made in this file will be lost!

from builtins import object

from qgis.PyQt import QtCore, QtGui, QtWidgets

try:
    _fromUtf8 = QtCore.QString.fromUtf8
except AttributeError:
    def _fromUtf8(s):
        return s

try:
    _encoding = QtWidgets.QApplication.UnicodeUTF8
    def _translate(context, text, disambig):
        return QtWidgets.QApplication.translate(context, text, disambig, _encoding)
except AttributeError:
    def _translate(context, text, disambig):
        return QtWidgets.QApplication.translate(context, text, disambig)

class Ui_changeDataSourceDialog(object):
    def setupUi(self, changeDataSourceDialog):
        changeDataSourceDialog.setObjectName(_fromUtf8("changeDataSourceDialog"))
        changeDataSourceDialog.resize(297, 305)
        self.verticalLayout = QtWidgets.QVBoxLayout(changeDataSourceDialog)
        self.verticalLayout.setObjectName(_fromUtf8("verticalLayout"))
        self.label_2 = QtWidgets.QLabel(changeDataSourceDialog)
        self.label_2.setObjectName(_fromUtf8("label_2"))
        self.verticalLayout.addWidget(self.label_2)
        self.selectDatasourceCombo = QtWidgets.QComboBox(changeDataSourceDialog)
        self.selectDatasourceCombo.setObjectName(_fromUtf8("selectDatasourceCombo"))
        self.verticalLayout.addWidget(self.selectDatasourceCombo)
        self.label = QtWidgets.QLabel(changeDataSourceDialog)
        self.label.setObjectName(_fromUtf8("label"))
        self.verticalLayout.addWidget(self.label)
        self.lineEdit = QtWidgets.QPlainTextEdit(changeDataSourceDialog)
        self.lineEdit.setObjectName(_fromUtf8("lineEdit"))
        self.verticalLayout.addWidget(self.lineEdit)
        self.openBrowser = QtWidgets.QPushButton(changeDataSourceDialog)
        self.openBrowser.setObjectName(_fromUtf8("openBrowser"))
        self.verticalLayout.addWidget(self.openBrowser)
        self.buttonBox = QtWidgets.QDialogButtonBox(changeDataSourceDialog)
        self.buttonBox.setOrientation(QtCore.Qt.Horizontal)
        self.buttonBox.setStandardButtons(QtWidgets.QDialogButtonBox.Cancel|QtWidgets.QDialogButtonBox.Ok)
        self.buttonBox.setObjectName(_fromUtf8("buttonBox"))
        self.verticalLayout.addWidget(self.buttonBox)

        self.retranslateUi(changeDataSourceDialog)
        QtCore.QMetaObject.connectSlotsByName(changeDataSourceDialog)

    def retranslateUi(self, changeDataSourceDialog):
        changeDataSourceDialog.setWindowTitle(_translate("changeDataSourceDialog", "undoLayerChanges", None))
        self.label_2.setText(_translate("changeDataSourceDialog", "Datasource Types", None))
        self.label.setText(_translate("changeDataSourceDialog", "URI:", None))
        self.openBrowser.setText(_translate("changeDataSourceDialog", "Browse", None))

