library(RMySQL)
library(shinydashboard)
library(shiny)
library(googleVis)
library(dplyr)
library(lubridate)
library(DT)
library(readr)
library(ggplot2)
mydb = dbConnect(MySQL(),user='readonly',password='readonly123',dbname='slcawdb',host='183.82.106.91')

########
rs = dbSendQuery(mydb, 'select sales_flat_order.updated_at, sales_flat_order.store_id, sales_flat_order.status,
                 sales_flat_invoice.created_at, sales_flat_invoice.grand_total,
                 sales_flat_invoice_item.qty, sales_flat_invoice_item.name, sales_flat_invoice_item.price
                 from sales_flat_order,sales_flat_invoice,sales_flat_invoice_item
                 where sales_flat_order.entity_id=sales_flat_invoice.order_id
                 and  sales_flat_invoice_item.entity_id=sales_flat_invoice.entity_id
                 and sales_flat_order.status = "complete"')
data = fetch(rs, n=-1)
data5=read.csv("data5.csv")
#Query to find AverageOrderValue
Avg_Order=dbSendQuery(mydb,'select sum(grand_total)/count(*) as AvgOrderValue from sales_flat_order where status="complete"')
Avg_Value=fetch(Avg_Order,n=-1)
Avg_Value=round(Avg_Value,2)
#Queryto find revenue
RevenueCal=dbSendQuery(mydb,'select sum(grand_total) from sales_flat_invoice')
Revenueval=fetch(RevenueCal,n=-1)
#Query to find tax amount
taxcal=dbSendQuery(mydb,"select sum(tax_amount) from sales_flat_invoice")
taxval=fetch(taxcal,n=-1)
#Query to find Repeatedcustomers
RepeatCust=dbSendQuery(mydb,'SELECT COUNT(*) AS grand_count FROM(
SELECT customer_email FROM sales_flat_order
                       WHERE sales_flat_order.status NOT LIKE "canceled"
                       AND sales_flat_order.status NOT LIKE "closed"
                       AND sales_flat_order.status NOT LIKE "fraud"
                       AND sales_flat_order.status NOT LIKE "holded"
                       AND sales_flat_order.status NOT LIKE "paypal_canceled_reversal"
                       GROUP BY customer_email HAVING COUNT(*) > 1
) s')
RepeatVal=fetch(RepeatCust,n=-1)
#Calculating E-Commerce Ratio
Visits=dbSendQuery(mydb,"select count(*) from log_visitor")
Visitsval=fetch(Visits,n=-1)
Transactions=dbSendQuery(mydb,"select count(*) from sales_flat_order where status='complete'")
TrValue=fetch(Transactions,n=-1)
ERatio=TrValue/Visitsval
##Revenue of a perticular month in all years

RevenueMarch=dbSendQuery(mydb,'select sum(grand_total) as Revenue ,Month(created_at) as Month,Year(created_at) as Year from sales_flat_order where Month(created_at) =3 group by  Year(created_at)')
RevenueMarchVal=fetch(RevenueMarch,n=-1)

##Sales of an item by location

saleLoc=dbSendQuery(mydb,'select sum(sales_flat_order.grand_total) as Revenue ,sales_flat_order_address.region as Location from 
                    sales_flat_order,sales_flat_order_address where sales_flat_order.entity_id=sales_flat_order_address.parent_id group by 
                    sales_flat_order_address.region,YEAR(sales_flat_order.created_at)=2015 order by (Month(sales_flat_order.created_at)=3)')
salelocval=fetch(saleLoc,n=-1)
##Sales of an item among  the year by location 
saleLocY=dbSendQuery(mydb,'select sum(sales_flat_order.grand_total) as Revenue ,sales_flat_order_address.region as Location from 
                    sales_flat_order,sales_flat_order_address where sales_flat_order.entity_id=sales_flat_order_address.parent_id group by 
                    sales_flat_order_address.region order by (YEAR(sales_flat_order.created_at)=2015)')
salelocvaly=fetch(saleLocY,n=-1)
##Sales of an item among  all the years by location 
saleLocYall=dbSendQuery(mydb,'select sum(sales_flat_order.grand_total) as Revenue ,sales_flat_order_address.region as Location from 
                     sales_flat_order,sales_flat_order_address where sales_flat_order.entity_id=sales_flat_order_address.parent_id group by 
                     sales_flat_order_address.region order by (YEAR(sales_flat_order.created_at))')
salelocvalyall=fetch(saleLocYall,n=-1)
##Revenue of items among all the years
yearwiserevenue=dbSendQuery(mydb,'select sum(grand_total) as Revenue ,
                            Month(created_at) as Month,
                            Year(created_at) as Year
                            from sales_flat_order 
                            
                            group by  Year
                            ')
RevenueByYear=fetch(yearwiserevenue,n=-1)

##### Top Product Sold in March ####  

ProductTop = dbSendQuery(mydb,'select sum(sales_flat_quote_item.qty) as TotalQty, 
                 sales_flat_quote_item.product_id, sales_flat_quote_item.name 
                 from sales_flat_quote_item,
                 sales_flat_order_item 
                 where YEAR(sales_flat_quote_item.updated_at) = 2016 AND MONTH(sales_flat_quote_item.updated_at)=3 AND
					       sales_flat_quote_item.product_id = sales_flat_order_item.product_id 
                 group by sales_flat_quote_item.product_id;')

topproduct= fetch(ProductTop, n=-1)  
maxQty<-max(topproduct$TotalQty)

####inventory Turnover=Cost of Goods Sold  /  Average Inventory 

turnover = dbSendQuery(mydb,'select sum(sales_flat_order.grand_total) as OrderTotal from
  sales_flat_order,sales_flat_invoice
                       where 
                       sales_flat_order.entity_id=sales_flat_invoice.order_id and
                       sales_flat_order.status="complete"')
Iturnover= fetch(turnover, n=-1)
inventurnover<-sum(Iturnover$OrderTotal)
###Revenue By Product Category
Rbycat=dbSendQuery(mydb,'SELECT sum(`qty`) as qty, sum(`total_price`) as Revenue, category_id FROM (SELECT sum(`qty_ordered`) as qty, sum(`row_total`) as total_price, sales_flat_order_item.product_id, catalog_category_product.category_id  FROM `sales_flat_order_item`
INNER JOIN catalog_category_product ON catalog_category_product.product_id = sales_flat_order_item.product_id
                   GROUP BY sales_flat_order_item.product_id order by catalog_category_product.category_id desc) as resTable GROUP BY category_id')
Rbycatval=fetch(Rbycat,n=-1)

######Inventory sales by location#####

Inventsales= dbSendQuery(mydb,'select sum(sales_flat_order.grand_total) as Revenue ,
                         sales_flat_order_address.region as Location,
                         (sales_flat_invoice.total_qty) as ordered_qunatity
                         
                         from sales_flat_order,sales_flat_order_address,sales_flat_invoice
                         
                         where sales_flat_order.entity_id=sales_flat_order_address.parent_id   
                         
                         and  sales_flat_order.entity_id=sales_flat_invoice.order_id                             
                         
                         group by sales_flat_order_address.region')


InventsalesbyRegion = fetch(Inventsales, n=-1)

##### units per transaction of the month march  ########
TotalUnitSold = dbSendQuery(mydb,'select sum(sales_flat_invoice_item.qty) as TotalQty
                            from sales_flat_invoice_item,sales_flat_order,sales_flat_invoice
                            where YEAR(sales_flat_order.updated_at)=2016 and MONTH(sales_flat_order.updated_at)=3 and
                            sales_flat_order.entity_id = sales_flat_invoice.order_id and 
                            sales_flat_invoice.entity_id=sales_flat_invoice_item.parent_id and
                            sales_flat_order.status="complete"')

TotalUnitsSold = fetch(TotalUnitSold, n=-1)
Transactions=dbSendQuery(mydb,'select count(*) from sales_flat_order where YEAR(updated_at)=2016 and MONTH(updated_at)=3 and status="complete";')
totalTransactions=fetch(Transactions,n=-1)
unitspertransaction<-TotalUnitsSold/totalTransactions
unitspertransaction<-round(unitspertransaction,2)

##### Visits per day ########

dVisits=dbSendQuery(mydb,"select count(*) from log_visitor where day(log_visitor.first_visit_at)=26 and month(log_visitor.first_visit_at)=3 and year(log_visitor.first_visit_at)=2016")
dVisitsperday=fetch(dVisits,n=-1)
