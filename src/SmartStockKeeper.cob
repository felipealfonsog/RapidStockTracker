IDENTIFICATION DIVISION.
PROGRAM-ID. RapidStockTracker.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 Option        PIC 9 VALUE 0.
01 ProductIndex  PIC 9 VALUE 0.
01 I             PIC 9 VALUE 0.
01 TotalValue    PIC 9(10)V99.

01 Product.
   05 ProductID     PIC X(10).
   05 ProductName   PIC X(30).
   05 Quantity      PIC 9(5).
   05 UnitPrice     PIC 9(7)V99.

01 InventoryTable.
   05 MaxProducts   PIC 99 VALUE 100.
   05 ProductEntry OCCURS 1 TO 100 TIMES
      DEPENDING ON MaxProducts.
      10 PRODUCT-ID      PIC X(10).
      10 PRODUCT-NAME    PIC X(30).
      10 PRODUCT-QTY     PIC 9(5).
      10 PRODUCT-PRICE   PIC 9(7)V99.

PROCEDURE DIVISION.
   PERFORM InitializeInventory.

   DISPLAY "Welcome to RapidStockTracker - Small Business Inventory Manager".
   PERFORM UNTIL Option = 4
      DISPLAY "1. Add Product".
      DISPLAY "2. View Inventory".
      DISPLAY "3. Generate Report".
      DISPLAY "4. Exit".
      ACCEPT Option.
      PERFORM ProcessOption.
   END-PERFORM.

   DISPLAY "Thank you for using RapidStockTracker. Goodbye!".

InitializeInventory.
   MOVE 0 TO ProductIndex.

ProcessOption.
   EVALUATE Option
      WHEN 1
         PERFORM AddProduct
      WHEN 2
         PERFORM ViewInventory
      WHEN 3
         PERFORM GenerateReport
      WHEN 4
         EXIT PROGRAM
      WHEN OTHER
         DISPLAY "Invalid Option. Please choose a valid option.".
   END-EVALUATE.

AddProduct.
   ACCEPT "Enter Product ID: "  ProductID.
   ACCEPT "Enter Product Name: "  ProductName.
   ACCEPT "Enter Quantity: "  Quantity.
   ACCEPT "Enter Unit Price: "  UnitPrice.

   ADD 1 TO ProductIndex.
   MOVE ProductID TO PRODUCT-ID(ProductIndex).
   MOVE ProductName TO PRODUCT-NAME(ProductIndex).
   MOVE Quantity TO PRODUCT-QTY(ProductIndex).
   MOVE UnitPrice TO PRODUCT-PRICE(ProductIndex).

   DISPLAY "Product added successfully.".

ViewInventory.
   IF ProductIndex = 0
      DISPLAY "Inventory is empty."
   ELSE
      DISPLAY "Inventory:".
      PERFORM VARYING I FROM 1 BY 1 UNTIL I > ProductIndex
         DISPLAY "Product ID: ", PRODUCT-ID(I).
         DISPLAY "Product Name: ", PRODUCT-NAME(I).
         DISPLAY "Quantity: ", PRODUCT-QTY(I).
         DISPLAY "Unit Price: $", PRODUCT-PRICE(I).
      END-PERFORM.
   END-IF.

GenerateReport.
   IF ProductIndex = 0
      DISPLAY "No products to generate a report."
   ELSE
      DISPLAY "Generating Report...".
      PERFORM VARYING I FROM 1 BY 1 UNTIL I > ProductIndex
         DISPLAY "Product ID: ", PRODUCT-ID(I).
         DISPLAY "Product Name: ", PRODUCT-NAME(I).
         DISPLAY "Quantity: ", PRODUCT-QTY(I).
         COMPUTE TotalValue = PRODUCT-QTY(I) * PRODUCT-PRICE(I).
         DISPLAY "Total Value: $", TotalValue.
      END-PERFORM.
   END-IF.

