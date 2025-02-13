# -*- coding: utf-8 -*-
"""
/***************************************************************************
 changeDataSource_je-vee
 Fork of:
 ***************************************************************************/

/***************************************************************************
 changeDataSource
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
from __future__ import print_function
from __future__ import absolute_import
from builtins import range
from builtins import object
import os.path

from PyQt5.QtCore import *
from PyQt5.QtGui import *
from PyQt5.QtXml import *
from PyQt5.QtWidgets import *
from qgis.core import *
from qgis.gui import QgsMessageBar

# Initialize Qt resources from file resources.py
from . import resources_rc
# Import the code for the dialog
from .changeDataSource_dialog import changeDataSourceDialog,dataSourceBrowser
from .setdatasource import setDataSource

# ==============================
layerColumnCount = 6

layerIdColumnNum = 0
layerNameColumnNum = 1
extensionColumnNum = 2
providerColumnNum = 3 # Was nr 2
dataSourceColumnNum = 4 # Was nr 3

extensionColumnWidth = 90
providerColumnWidth = 80
# ==============================

class changeDataSource(object):
    """QGIS Plugin Implementation."""

    def __init__(self, iface):
        """Constructor.

        :param iface: An interface instance that will be passed to this class
            which provides the hook by which you can manipulate the QGIS
            application at run time.
        :type iface: QgsInterface
        """
        # Save reference to the QGIS interface
        self.iface = iface
        # initialize plugin directory
        self.plugin_dir = os.path.dirname(__file__)
        # initialize locale
        locale = QSettings().value('locale/userLocale')[0:2]
        locale_path = os.path.join(
            self.plugin_dir,
            'i18n',
            'changeDataSource_{}.qm'.format(locale))

        if os.path.exists(locale_path):
            self.translator = QTranslator()
            self.translator.load(locale_path)

            if qVersion() > '4.3.3':
                QCoreApplication.installTranslator(self.translator)

        # Create the dialog (after translation) and keep reference
        self.dlg = changeDataSourceDialog()

        # Adds button to maximise the dialogue and enables resizing
        self.dlg.setWindowFlags(Qt.Window | Qt.WindowMaximizeButtonHint | Qt.WindowCloseButtonHint)
        self.dlg.setSizeGripEnabled(True)

        # Declare instance attributes
        self.actions = []
        self.menu = self.tr('&changeDataSource_je-vee')

        # OLD TODO: We are going to let the user set this up in a future iteration
        self.toolbar = self.iface.addToolBar('changeDataSource_je-vee')
        self.toolbar.setObjectName('changeDataSource_je-vee')

    # noinspection PyMethodMayBeStatic
    def tr(self, message):
        """Get the translation for a string using Qt translation API.

        We implement this ourselves since we do not inherit QObject.

        :param message: String for translation.
        :type message: str, QString

        :returns: Translated version of message.
        :rtype: QString
        """
        # noinspection PyTypeChecker,PyArgumentList,PyCallByClass
        return QCoreApplication.translate('changeDataSource_je-vee', message)

    def add_action(
        self,
        icon_path,
        text,
        callback,
        enabled_flag=True,
        add_to_menu=True,
        add_to_toolbar=True,
        status_tip=None,
        whats_this=None,
        parent=None):
        """Add a toolbar icon to the toolbar.

        :param icon_path: Path to the icon for this action. Can be a resource
            path (e.g. ':/plugins/foo/bar.png') or a normal file system path.
        :type icon_path: str

        :param text: Text that should be shown in menu items for this action.
        :type text: str

        :param callback: Function to be called when the action is triggered.
        :type callback: function

        :param enabled_flag: A flag indicating if the action should be enabled
            by default. Defaults to True.
        :type enabled_flag: bool

        :param add_to_menu: Flag indicating whether the action should also
            be added to the menu. Defaults to True.
        :type add_to_menu: bool

        :param add_to_toolbar: Flag indicating whether the action should also
            be added to the toolbar. Defaults to True.
        :type add_to_toolbar: bool

        :param status_tip: Optional text to show in a popup when mouse pointer
            hovers over the action.
        :type status_tip: str

        :param parent: Parent widget for the new action. Defaults None.
        :type parent: QWidget

        :param whats_this: Optional text to show in the status bar when the
            mouse pointer hovers over the action.

        :returns: The action that was created. Note that the action is also
            added to self.actions list.
        :rtype: QAction
        """

        icon = QIcon(icon_path)
        action = QAction(icon, text, parent)
        action.triggered.connect(callback)
        action.setEnabled(enabled_flag)

        if status_tip is not None:
            action.setStatusTip(status_tip)

        if whats_this is not None:
            action.setWhatsThis(whats_this)

        if add_to_toolbar:
            self.toolbar.addAction(action)

        if add_to_menu:
            self.iface.addPluginToVectorMenu(
                self.menu,
                action)

        self.actions.append(action)

        return action

    def initGui(self):
        """Create the menu entries and toolbar icons inside the QGIS GUI."""

        icon_path = os.path.join(self.plugin_dir,"icon.png")
        self.add_action(
            icon_path,
            text=self.tr('changeDataSource_je-vee'),
            callback=self.run,
            parent=self.iface.mainWindow())
        self.changeDSActionVector = QAction(
            QIcon(os.path.join(self.plugin_dir, "icon.png")),
            "Change vector datasource",
            self.iface,
        )
        self.changeDSActionRaster = QAction(
            QIcon(os.path.join(self.plugin_dir, "icon.png")),
            "Change raster datasource",
            self.iface,
        )
        self.iface.addCustomActionForLayerType(
            self.changeDSActionVector, "", QgsMapLayer.VectorLayer, True
        )
        self.iface.addCustomActionForLayerType(
            self.changeDSActionRaster, "", QgsMapLayer.RasterLayer, True
        )
        self.changeDSTool = setDataSource(self, )
        self.browserDialog = dataSourceBrowser()
        # self.dlg.hideNonOGRCheckbox.hide()
        # self.dlg.handleBadLayersCheckbox.hide()
        # self.dlg.reconcileButton.hide()

        self.connectSignals()
        self.session = 0

    def connectSignals(self):
        """
        Connect all the buttons to actions
        """
        self.changeDSActionVector.triggered.connect(self.changeLayerDS)
        self.changeDSActionRaster.triggered.connect(self.changeLayerDS)
        self.dlg.replaceButton.clicked.connect(self.replaceDS)
        self.dlg.layerTable.verticalHeader().sectionClicked.connect(self.activateSelection)
        self.dlg.buttonBox.button(QDialogButtonBox.Reset).clicked.connect(lambda: self.buttonBoxHub("Reset"))
        self.dlg.buttonBox.button(QDialogButtonBox.Apply).clicked.connect(lambda: self.buttonBoxHub("Apply"))
        self.dlg.buttonBox.button(QDialogButtonBox.Cancel).clicked.connect(lambda: self.buttonBoxHub("Cancel"))
        self.dlg.closedDialog.connect(self.removeServiceLayers)
        self.dlg.hideNonOGRCheckbox.stateChanged.connect(self.hideNonOGRCheckboxOption)
        self.iface.newProjectCreated.connect(self.updateSession)

        # Originally commented out => not functioning
        # self.dlg.reconcileButton.clicked.connect(self.reconcileUnhandled)
        # self.dlg.handleBadLayersCheckbox.stateChanged.connect(self.handleBadLayerOption)
        # self.iface.initializationCompleted.connect(self.initHandleBadLayers)
        # self.iface.projectRead.connect(self.recoverUnhandledLayers)
        # self.initHandleBadLayers()

    def setEmbeddedLayer(self,layer):
        """
        Set the layer as embedded layer in the layer tree.
        This means the layer will not be shown in the layer tree
        but will be kept in the project
        :param layer: The layer to set as embedded.
        :type layer: QgsMapLayer
        """
        root = QgsProject.instance().layerTreeRoot()
        layerNode = root.findLayer(layer.id())
        layerNode.setCustomProperty("embedded","")

    def updateSession(self):
        """
        Increases the session number by one. This is used to keep track of how many times a project has been loaded or saved
        """
        self.session += 1

    def activateSelection(self):
        indexes = []
        for selectionRange in self.dlg.layerTable.selectedRanges():
            indexes.extend(list(range(selectionRange.topRow(), selectionRange.bottomRow()+1)))
        if indexes:
            self.dlg.onlySelectedCheck.setChecked(True)
        else:
            self.dlg.onlySelectedCheck.setChecked(False)

    def changeLayerDS(self):
        self.dlg.hide()
        self.changeDSTool.openDataSourceDialog(
            self.iface.layerTreeView().currentLayer()
        ) # , self.badLayersHandler)

    def unload(self):
        """
        Disconnects from signals. Removes the plugin menu item and icon from QGIS GUI
        """
        self.iface.removeCustomActionForLayerType(self.changeDSActionVector)
        self.iface.removeCustomActionForLayerType(self.changeDSActionRaster)

        for action in self.actions:
            self.iface.removePluginVectorMenu(
                self.tr('&changeDataSource_je-vee'),
                action)
            self.iface.removeToolBarIcon(action)
        # remove the toolbar
        del self.toolbar

    def populateLayerTable(self):
        '''
        Method to write layer info in layer table
        '''
        self.changeDSTool.populateComboBox(
            self.dlg.datasourceCombo,
            [""]
            + list(self.changeDSTool.vectorDSList.keys())
            + list(self.changeDSTool.rasterDSList.keys()),
        )
        self.dlg.layerTable.clear()
        for row in range(self.dlg.layerTable.rowCount()):
            self.dlg.layerTable.removeRow(row)
        self.dlg.layerTable.setRowCount(0)

        self.dlg.layerTable.setColumnCount(layerColumnCount)
        self.dlg.layerTable.setHorizontalHeaderItem(0,QTableWidgetItem("ID"))
        self.dlg.layerTable.setHorizontalHeaderItem(1,QTableWidgetItem("Layer Name"))
        self.dlg.layerTable.setHorizontalHeaderItem(2,QTableWidgetItem("Extension"))
        self.dlg.layerTable.setHorizontalHeaderItem(3,QTableWidgetItem("Provider"))
        self.dlg.layerTable.setHorizontalHeaderItem(4,QTableWidgetItem("Data Source"))
        self.dlg.layerTable.setHorizontalHeaderItem(5,QTableWidgetItem(""))

        layersPropLayerDef = "Point?crs=epsg:3857&field=layerid:string(200)&field=layername:string(200)&field=layertype:string(20)&field=geometrytype:string(20)&field=provider:string(20)&field=datasource:string(250)&field=authid:string(20)&field=source_ext:string(20)"

        # Changed layername to be more distinguishable and avoid any conflicts when removing
        layerTableName = "cDS_layerTable"

        self.layersPropLayer = QgsVectorLayer(layersPropLayerDef, layerTableName,"memory")
        dummyFeatures = []

        self.dlg.layerTable.horizontalHeader().setDefaultAlignment(Qt.AlignLeft)

        self.dlg.layerTable.horizontalHeader().setSectionsClickable(False)

        self.dlg.layerTable.hideColumn(0)
        self.dlg.layerTable.hideColumn(layerColumnCount)
        self.dlg.layerTable.hideColumn(layerColumnCount+1)
        # Get the instance of the project
        projectInstance = QgsProject.instance()

        # Retrieve all layers from the project
        allLayers = projectInstance.mapLayers().values()

        # Separate layers into those with 'ogr' provider and others
        ogrLayers = []
        nonOgrLayers = []

        for layer in allLayers:
            # Checks that the layer is not the same as this script creates
            if layer.name() == layerTableName:
                # If found, removes from project
                # TODO: Check performance hit and possibly remove
                projectInstance.removeMapLayer(layer.id())
            # Checks to consider only Vectors and Rasters
            elif layer.type() in (QgsMapLayer.VectorLayer, QgsMapLayer.RasterLayer):
                provider = layer.dataProvider().name()
                if provider:
                    # Stores layer, provider and source
                    obj = {"layer": layer, "provider_name": provider, "source": layer.source()}

                    # Separation
                    if provider == 'ogr':
                        ogrLayers.append(obj)
                    else:
                        nonOgrLayers.append(obj)

        # Sort layers with 'ogr' provider by name, then source
        ogrLayers.sort(key=lambda l: (l.get("name"), l.get("source")))

        # Sort non-'ogr' layers first by source provider name, name, then source
        nonOgrLayers.sort(key=lambda l: (l.get("provider_name"), l.get("name"), l.get("source")))

        # Combine the sorted layers back together
        combinedLayers = ogrLayers + nonOgrLayers

        # Checks not needed since they happen earlier
        for layer in combinedLayers:
            layerOriginal = layer
            layer = layer.get("layer")
            provider = layerOriginal.get("provider_name") 
            provider = provider if provider != 'wms' else provider.upper()
            source = layerOriginal.get("source")
            source_ext = os.path.splitext(source)[1] if provider == 'ogr' else 'WEB'
            source_ext = source_ext.split('|')[0] if '|' in source_ext else source_ext
            cellStyle = ""
            # print(layer.id())

            lastRow = self.dlg.layerTable.rowCount()
            self.dlg.layerTable.insertRow(lastRow)
            # Sets layer-id
            # (No column name, is hidden later)
            self.dlg.layerTable.setCellWidget(lastRow,0,self.getLabelWidget(layer.id(),0,style = cellStyle))
            # Sets layer name
            # Column "Layer Name"
            self.dlg.layerTable.setCellWidget(lastRow,1,self.getLabelWidget(str(layer.name()),1,style = cellStyle))

            # Column "Extension"
            self.dlg.layerTable.setCellWidget(lastRow,2,self.getLabelWidget(str(source_ext),2,style = cellStyle))

            # Sets provider
            # Column "Provider"
            self.dlg.layerTable.setCellWidget(lastRow,3,self.getLabelWidget(str(provider),3,style = cellStyle))
            # Sets source
            # Column "Data Source"
            self.dlg.layerTable.setCellWidget(lastRow,4,self.getLabelWidget(str(source),4,style = cellStyle))
            # Sets the last button to browse the folder?
            self.dlg.layerTable.setCellWidget(lastRow,5,self.getButtonWidget(lastRow))

            layerDummyFields = self.layersPropLayer.fields()
            # layerDummyFields.append(QgsField("source_ext", QVariant.String))

            layerDummyFeature = QgsFeature(layerDummyFields)
            if layer.type() == QgsMapLayer.VectorLayer:
                layerType = "vector"
                enumGeometryTypes =('Point','Line','Polygon','UnknownGeometry','NoGeometry')
                geometry = enumGeometryTypes[layer.geometryType()]
            else:
                layerType = "raster"
                geometry = ""
            dummyGeometry = QgsGeometry.fromPointXY(self.iface.mapCanvas().center())
            layerDummyFeature.setGeometry(dummyGeometry)
            layerDummyFeature.setAttributes(
                [
                    layer.id(),
                    layer.name(),
                    layerType,
                    geometry,
                    provider,
                    source,
                    layer.crs().authid(),
                    source_ext,
                ]
            )
            dummyFeatures.append(layerDummyFeature)

        self.layersPropLayer.dataProvider().addFeatures(dummyFeatures)
        projectInstance.addMapLayer(self.layersPropLayer)
        QgsProject.instance().layerTreeRoot().findLayer(self.layersPropLayer.id()).setItemVisibilityChecked(False)

        self.dlg.mFieldExpressionWidget.setLayer(self.layersPropLayer)
        # Layer name column
        self.dlg.layerTable.resizeColumnToContents(layerNameColumnNum)
        # Extension column
        # TODO: Store in variable
        self.dlg.layerTable.setColumnWidth(extensionColumnNum, extensionColumnWidth)
        self.dlg.layerTable.horizontalHeader().setSectionResizeMode(extensionColumnNum,QHeaderView.Fixed)
        # Provider column
        # self.dlg.layerTable.horizontalHeader().setSectionResizeMode(providerColumnNum,QHeaderView.ResizeToContents)
        # self.dlg.layerTable.resizeColumnToContents(providerColumnNum)
        self.dlg.layerTable.setColumnWidth(providerColumnNum, providerColumnWidth)
        self.dlg.layerTable.horizontalHeader().setSectionResizeMode(providerColumnNum,QHeaderView.Fixed) # was QHeaderView.Stretch
        
        # Data Source column
        self.dlg.layerTable.setColumnWidth(dataSourceColumnNum,30)
        self.dlg.layerTable.horizontalHeader().setSectionResizeMode(dataSourceColumnNum,QHeaderView.Stretch) # was QHeaderView.Stretch

        # TODO: ?
        self.dlg.layerTable.setShowGrid(True)
        # TODO: Make the columns sortable and filterable?
        # Trying to make the table sortable
        # self.dlg.layerTable.setSortingEnabled(True)

    def getButtonWidget(self,row):
        """
        Creates a button widget for the specified row in the layer table.

        The button is labeled with '...' and is configured to trigger the browseAction 
        method when clicked. The button's size policy is set to Ignored for both 
        horizontal and vertical directions

        :param row: The row index in the layer table for which the button is created.
        :type row: int
        :return: A QPushButton widget configured to invoke browseAction on click.
        :rtype: QPushButton
        """
        edit = QPushButton("...",parent = self.dlg.layerTable)
        edit.setSizePolicy(QSizePolicy.Ignored,QSizePolicy.Ignored)
        edit.clicked.connect(lambda: self.browseAction(row))
        return edit

    def browseAction(self,row):
        '''
        Method to open qgis browser dialog to get new datasource/provider
        '''
        layerId = self.dlg.layerTable.cellWidget(row, layerIdColumnNum).text()
        layerName = self.dlg.layerTable.cellWidget(row, layerNameColumnNum).text()
        newType,newProvider,newDatasource = dataSourceBrowser.uri(title = layerName)
        # check if databrowser return a incompatible layer type
        rowLayer = QgsProject.instance().mapLayer(layerId)
        enumLayerTypes = ("vector","raster","plugin")
        if newType and enumLayerTypes[rowLayer.type()] != newType:
            self.iface.messageBar().pushMessage(
                "Error",
                "Layer type mismatch %s/%s"
                % (enumLayerTypes[rowLayer.type()], newType),
                level=QgsMessageBar.CRITICAL,
                duration=4,
            )
            return None
        if newDatasource:
            self.dlg.layerTable.cellWidget(row, dataSourceColumnNum).setText(newDatasource)
        if newProvider:
            self.dlg.layerTable.cellWidget(row, providerColumnNum).setText(newProvider)

    def getLabelWidget(self,txt,column, style = None):
        '''
        Method that returns a preformatted qlineedit widget
        '''
        edit = QLineEdit(parent = self.dlg.layerTable)
        idealWidth = QApplication.instance().fontMetrics().width(txt)
        edit.setMinimumWidth(idealWidth)
        if column == extensionColumnNum:
            edit.setMaximumWidth(extensionColumnWidth)
        if column == providerColumnNum:
            edit.setMaximumWidth(providerColumnWidth)
        edit.setText(txt)
        edit.setSizePolicy(QSizePolicy.Minimum,QSizePolicy.Ignored)
        if style:
            edit.setStyleSheet(style)
        else:
            edit.setStyleSheet("QLineEdit{background: rgba(0,190,0, 0%);}")
        edit.column = column
        edit.changed = None
        if column == layerNameColumnNum:
            edit.setReadOnly(True)
        else:
            edit.textChanged.connect(lambda: self.highlightCell(edit,"QLineEdit{background: yellow;}"))
        edit.setCursorPosition(0)
        return edit

    def highlightCell(self,cell,newStyle):
        '''
        Method to highlight a cell in the table, setting a new stylesheet on the cell and marking it as changed
        :param cell: the cell to be highlighted
        :param newStyle: the new style to be applied to the cell
        '''
        cell.setStyleSheet(newStyle)
        cell.changed = True

    def replaceDS(self):
        '''
        Method to replace the datasource string accordind to find/replace string or to expression result if valid
        '''
        self.replaceList=[]
        indexes = []
        context = QgsExpressionContext()
        scope = QgsExpressionContextScope()
        context.appendScope(scope)
        # build replace list
        if self.dlg.onlySelectedCheck.isChecked():
            for selectionRange in self.dlg.layerTable.selectedRanges():
                indexes.extend(list(range(selectionRange.topRow(), selectionRange.bottomRow()+1)))
            for row in indexes:
                self.replaceList.append(QgsProject.instance().mapLayer(self.dlg.layerTable.cellWidget(row, layerIdColumnNum).text()))
        else:
            for row in range(0,self.dlg.layerTable.rowCount()):
                indexes.append(row)
                self.replaceList.append(QgsProject.instance().mapLayer(self.dlg.layerTable.cellWidget(row, layerIdColumnNum).text()))
        for row in indexes:
            # layerId = self.dlg.layerTable.cellWidget(row, layerIdColumnNum) # => TODO: Unused variable
            cell = self.dlg.layerTable.cellWidget(row, dataSourceColumnNum)
            # orig = cell.text() # => TODO: Unused variable
            if self.dlg.mFieldExpressionWidget.isValidExpression():
                exp = QgsExpression(self.dlg.mFieldExpressionWidget.currentText())
                scope.setFeature(next(self.layersPropLayer.getFeatures(QgsFeatureRequest(row+1))))
                expResult = exp.evaluate(context)
                cell.setText(expResult)
            else:
                cell.setText(cell.text().replace(self.dlg.findEdit.text(),self.dlg.replaceEdit.text()))
            if self.dlg.datasourceCombo.currentText() != "":
                self.dlg.layerTable.cellWidget(row, providerColumnNum).setText(self.dlg.datasourceCombo.currentText())

    def applyDSChanges(self):#, reconcileUnhandled = False):
        '''
        Method to scan table row and apply the provider/datasource strings if changed
        '''

        for row in range(0,self.dlg.layerTable.rowCount()):
            rowProviderCell = self.dlg.layerTable.cellWidget(row, providerColumnNum)
            rowDatasourceCell = self.dlg.layerTable.cellWidget(row, dataSourceColumnNum)
            rowLayerID = self.dlg.layerTable.cellWidget(row, layerIdColumnNum).text()
            # rowLayerName = self.dlg.layerTable.cellWidget(row, layerNameColumnNum).text() # => TODO: Unused variable
            rowProvider = rowProviderCell.text()
            rowDatasource = rowDatasourceCell.text()
            rowLayer = QgsProject.instance().mapLayer(rowLayerID)

            rowProviderChanging = rowProviderCell.changed
            rowDatasourceChanging = rowDatasourceCell.changed

            if rowProviderChanging or rowDatasourceChanging:
                # fix_print_with_import
                print(("ROWS",rowLayer,rowProvider,rowDatasource))
                if self.changeDSTool.applyDataSource(rowLayer,rowProvider,rowDatasource):
                    resultStyle = "QLineEdit{background: green;}"
                else:
                    resultStyle = "QLineEdit{background: red;}"
                if rowProviderChanging:
                    rowProviderCell.setStyleSheet(resultStyle)
                if rowDatasourceChanging:
                    rowDatasourceCell.setStyleSheet(resultStyle)

    def removeServiceLayers(self):
        '''
        Method to remove service properties layer, used for expression changes
        and unhandled layers group if empty
        '''
        # fix_print_with_import
        print("removing")
        try:
            QgsProject.instance().removeMapLayer(self.layersPropLayer.id())
        except:
            pass

    def hideNonOGRCheckboxOption(self, state):
        '''
        Method to hide or show non OGR layers according to checkbox
        '''
        if state == Qt.Checked:
            self.hideNonOGR()
        else:
            self.showAll()

    def hideNonOGR(self):
        '''
        Method to hide non OGR layers
        '''
        for row in range(self.dlg.layerTable.rowCount()):
            layerId = self.dlg.layerTable.cellWidget(row, 0).text()
            rowLayer = QgsProject.instance().mapLayer(layerId)
            if rowLayer.providerType() != 'ogr':
                self.dlg.layerTable.hideRow(row)

    def showAll(self):
        '''
        Method to show all layers
        '''
        for row in range(self.dlg.layerTable.rowCount()):
            self.dlg.layerTable.showRow(row)


    def buttonBoxHub(self,kod):
        '''
        Method to handle button box clicking
        '''
        # fix_print_with_import
        print(kod)
        if kod == "Reset":
            # fix_print_with_import
            print("reset")
            self.removeServiceLayers()
            self.populateLayerTable()
        elif kod == "Cancel":
            self.removeServiceLayers()
            self.dlg.hide()
        elif kod == "Apply":
            self.applyDSChanges()

    def reconcileUnhandled(self):
        '''
        Method to reconcile unhandled layers by applying changes to the data source.
        This method calls the applyDSChanges function with the reconcileUnhandled
        parameter set to True, indicating that the changes should account for any
        unhandled layers that need reconciliation.
        '''
        self.applyDSChanges(reconcileUnhandled = True)

    def run(self):
        """Run method that performs all the real work"""
        # show the dialog
        if not self.dlg.isVisible():
            self.populateLayerTable()

            self.dlg.show()
            self.dlg.raise_()
            self.dlg.activateWindow()
            # Run the dialog event loop
            result = self.dlg.exec_()
            # See if OK was pressed
            if result:
                # Do something useful here - delete the line containing pass and
                # substitute with your code.
                pass
        else:
            self.dlg.raise_()


class browseLineEdit(QLineEdit):
    '''
    Class to provide custom resizable lineedit
    '''
    buttonClicked = pyqtSignal(bool)

    def __init__(self, parent=None):
        super(browseLineEdit, self).__init__(parent)

        self.button = QToolButton(self)
        self.button.setIcon(QIcon(os.path.join(os.path.dirname(__file__),"BrowseButton.png")))
        self.button.setStyleSheet('border: 0px; padding: 0px;')
        self.button.setCursor(Qt.ArrowCursor)
        self.button.clicked.connect(self.buttonClicked.emit)

        frameWidth = self.style().pixelMetric(QStyle.PM_DefaultFrameWidth)
        buttonSize = self.button.sizeHint()

        self.setStyleSheet('QLineEdit {padding-left: %dpx; }' % (buttonSize.width() + frameWidth + 1))
        self.setMinimumSize(
            max(
                self.minimumSizeHint().width(), buttonSize.width() + frameWidth * 2 + 2
            ),
            max(
                self.minimumSizeHint().height(),
                buttonSize.height() + frameWidth * 2 + 2,
            ),
        )

    def resizeEvent(self, event):
        """
        Override the resizeEvent to reposition the button within the QLineEdit

        This method is called whenever the browseLineEdit is resized. It calculates
        the position of the embedded button based on the current size of the line
        edit, ensuring that the button is correctly aligned to the right side of
        the line edit

        :param event: The resize event that triggered this method
        :type event: QResizeEvent
        """
        buttonSize = self.button.sizeHint()
        frameWidth = self.style().pixelMetric(QStyle.PM_DefaultFrameWidth)
        self.button.move(
            self.rect().right() - frameWidth - buttonSize.width(),
            (self.rect().bottom() - buttonSize.height() + 1) / 2,
        )
        super(browseLineEdit, self).resizeEvent(event)
