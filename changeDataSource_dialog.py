# -*- coding: utf-8 -*-
"""
/***************************************************************************
 changeDataSourceDialog
                                 A QGIS plugin
 right click on layer tree to change layer datasource
                             -------------------
        begin                : 2015-09-29
        git sha              : $Format:%H$
        copyright            : (C) 2015 by enrico ferreguti
        email                : enricofer@gmail.com
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
"""
from __future__ import absolute_import

from qgis.PyQt import QtGui, uic, QtWidgets
from qgis.PyQt.QtCore import pyqtSignal
from qgis.core import QgsBrowserModel, QgsMimeDataUtils

from .changeDataSource_dialog_base import Ui_changeDataSourceDialogBase
from .browsedatasource import Ui_dataSourceBrowser

class changeDataSourceDialog(QtWidgets.QDialog, Ui_changeDataSourceDialogBase):

    def __init__(self, parent=None):
        """Constructor."""
        super(changeDataSourceDialog, self).__init__(parent)
        # QtWidgets.QDialog.__init__(self)
        # Set up the user interface from Designer.
        # After setupUI you can access any designer object by doing
        # self.<objectname>, and you can use autoconnect slots - see
        # http://qt-project.org/doc/qt-4.8/designer-using-a-ui-file.html
        # widgets-and-dialogs-with-auto-connect
        self.setupUi(self)

    closedDialog = pyqtSignal()

    def closeEvent(self, inEvent):
        """
        Overrides the default closeEvent method in a dialog to emit a custom closedDialog signal when the dialog is closed
        """
        self.closedDialog.emit()

class dataSourceBrowser(QtWidgets.QDialog, Ui_dataSourceBrowser):

    def __init__(self, parent=None):
        """Constructor."""
        # TODO: Change to newer file browser
        super(dataSourceBrowser, self).__init__(parent)
        # QtWidgets.QDialog.__init__(self)
        # Set up the user interface from Designer.
        # After setupUI you can access any designer object by doing
        # self.<objectname>, and you can use autoconnect slots - see
        # http://qt-project.org/doc/qt-4.8/designer-using-a-ui-file.html
        # widgets-and-dialogs-with-auto-connect
        self.setupUi(self)
        self.browserModel = QgsBrowserModel()
        self.browserModel.initialize()
        self.dataSourceTree.setModel(self.browserModel)
        self.dataSourceTree.doubleClicked.connect(self.getUriFromBrowser)
        self.dataSourceTree.header().hide()
        self.hide()
        self.buttonBox.accepted.connect(self.acceptedAction)
        self.buttonBox.rejected.connect(self.rejectedAction)
        self.acceptedFlag = None

    def getUriFromBrowser(self, index):
        """
        Retrieves the URI from the selected item in the browser.

        :param index: The index of the selected item in the browser model.
        :type index: QModelIndex
        :raises: Sets self.result to a tuple of (layerType, providerKey, uri) if successful,
                otherwise sets to (None, None, None) if there's an error or no valid URI.
        """
        uriItem = self.browserModel.dataItem(index)
        uriList = QgsMimeDataUtils.decodeUriList(self.browserModel.mimeData([index]))
        try:
            # print uriList[0].providerKey, uriList[0].uri
            self.result = (uriList[0].layerType, uriList[0].providerKey, uriList[0].uri)
            self.close()
            self.acceptedFlag = True
        except:
            # print "NO VALID URI"
            self.result = (None, None, None)

    def acceptedAction(self):
        self.getUriFromBrowser(self.dataSourceTree.currentIndex())
        self.close()
        self.acceptedFlag = True

    def rejectedAction(self):
        self.close()
        self.acceptedFlag = None

    @staticmethod
    def uri(title=""):
        dialog = dataSourceBrowser()
        dialog.setWindowTitle(title)
        result = dialog.exec_()
        dialog.show()
        if dialog.acceptedFlag:
            return dialog.result
        else:
            return (None, None, None)
